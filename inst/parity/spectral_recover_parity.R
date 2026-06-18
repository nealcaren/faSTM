#!/usr/bin/env Rscript
# HISTORICAL (pre-topica-v0.24.0). This script ports topica's OLD recover() — a
# fixed-iteration, non-converging exponentiated-gradient solve — to show it did
# NOT reproduce R stm's recoverL2() (aligned cosine ~0.36). topica v0.24.0
# (#234/#235) FIXED this: the recover() step now uses a scale-adaptive,
# converged update and reproduces recoverL2() at cosine 1.0 (see topica's
# parity/spectral_recover_stm.py). Kept for provenance.
#
# IMPORTANT (then and now): matching the recovery *step* does not mean faSTM
# reproduces stm's *fitted* topics — the EM objective is non-convex, so
# independent fits land in different basins. For a guaranteed replication, start
# faSTM from stm's own spectral beta: stm(..., init.beta = <K x V>).
#
# Original description follows -------------------------------------------------
# Minimal, self-contained reproduction: topica's spectral-init RECOVERY step
# does not reproduce R stm's recoverL2(), even on an identical co-occurrence
# matrix and identical anchor words.
#
# Needs only: R + the `stm` and `Matrix` packages. topica's recover() is ported
# faithfully from src/spectral.rs (the published Rust source) so the comparison
# runs without building topica.
#
#   Rscript spectral_recover_parity.R
#
# What it shows, in order:
#   1. topica's Qbar == stm's gram() Qbar           (max abs diff ~1e-15)
#   2. topica's anchors == stm's fastAnchor anchors  (identical word set)
#   3. topica's recover() != stm's recoverL2()       (aligned cosine ~0.2)
#      and topica's recover() does not converge to stm's solution as iters grow
#      (the exp-gradient at eta=50 / fixed iteration count is non-convergent).

suppressMessages({library(stm); library(Matrix)})

K <- 5L
data("gadarian", package = "stm")
proc <- textProcessor(gadarian$open.ended.response, metadata = gadarian, verbose = FALSE)
out  <- prepDocuments(proc$documents, proc$vocab, proc$meta, verbose = FALSE)
docs <- out$documents; vocab <- out$vocab; V <- length(vocab)

## doc x V count matrix for stm:::gram
rows <- integer(0); cols <- integer(0); vals <- integer(0)
for (i in seq_along(docs)) { m <- docs[[i]]
  rows <- c(rows, rep(i, ncol(m))); cols <- c(cols, m[1, ]); vals <- c(vals, m[2, ]) }
mat <- sparseMatrix(i = rows, j = cols, x = vals, dims = c(length(docs), V))

## --- stm path ---
Q <- stm:::gram(mat); Qsums <- rowSums(Q); Qbar_stm <- Q / Qsums
anchors_stm <- stm:::fastAnchor(Qbar_stm, K, verbose = FALSE)

## --- topica path (faithful port of src/spectral.rs) ---
# cooccurrence(): per-doc 1/(n(n-1)) weighting with the c_i(c_i-1) diagonal, then
# /used_docs (a global scalar that cancels under row-normalization), then row-norm.
q <- matrix(0, V, V); used <- 0
for (m in docs) {
  ids <- m[1, ]; cts <- as.numeric(m[2, ]); n <- sum(cts)
  if (n < 2) next; used <- used + 1; nrm <- 1 / (n * (n - 1))
  for (a in seq_along(ids)) for (b in seq_along(ids)) {
    val <- if (ids[a] == ids[b]) cts[a] * (cts[a] - 1) else cts[a] * cts[b]
    q[ids[a], ids[b]] <- q[ids[a], ids[b]] + val * nrm
  }
}
q <- q / used; p_topica <- rowSums(q)
Qbar_topica <- q; rs <- rowSums(q); Qbar_topica[rs > 0, ] <- q[rs > 0, ] / rs[rs > 0]

topica_anchors <- function(qbar, K) {
  resid <- qbar; anchors <- which.max(rowSums(qbar^2))
  gs <- function(resid, pos) { b <- resid[pos, ]; nb <- sqrt(sum(b^2))
    if (nb > 1e-12) { b <- b / nb; resid <- resid - (resid %*% b) %*% t(b) }; resid }
  resid <- gs(resid, anchors[1])
  while (length(anchors) < K) {
    rn <- rowSums(resid^2); rn[anchors] <- -Inf
    pos <- which.max(rn); if (rn[pos] < 1e-12) break
    anchors <- c(anchors, pos); resid <- gs(resid, pos)
  }
  anchors
}
anchors_topica <- topica_anchors(Qbar_topica, K)

# recover(): exp-gradient on the simplex, eta=50, FIXED `iters`, no convergence
# check, no anchor special-casing (the three differences from stm::recoverL2).
topica_recover <- function(Qbar, anchors, p, K, V, iters = 120) {
  A <- Qbar[anchors, , drop = FALSE]; G <- A %*% t(A); amat <- matrix(0, V, K)
  for (w in 1:V) {
    b <- as.numeric(A %*% Qbar[w, ]); cc <- rep(1 / K, K)
    for (it in 1:iters) {
      gc <- as.numeric(G %*% cc)
      cc <- cc * exp(-50 * 2 * (gc - b)); cc[!is.finite(cc)] <- 0
      s <- sum(cc); cc <- if (s > 0) cc / s else rep(1 / K, K)
    }
    amat[w, ] <- cc * p[w]
  }
  beta <- matrix(0, K, V)
  for (t in 1:K) { col <- sum(amat[, t]); beta[t, ] <- (amat[, t] + 1e-8) / (col + V * 1e-8) }
  beta
}

cos_align <- function(a, b) {
  an <- a / sqrt(rowSums(a^2)); bn <- b / sqrt(rowSums(b^2)); sim <- an %*% t(bn)
  k <- nrow(a); u <- logical(k); tot <- 0
  for (i in order(-apply(sim, 1, max))) { j <- which.max(ifelse(u, -Inf, sim[i, ])); u[j] <- TRUE; tot <- tot + sim[i, j] }
  tot / k
}

## --- results ---
cat(sprintf("1. Qbar  max|topica - stm|      = %.2e   (identical)\n",
            max(abs(Qbar_topica - Qbar_stm))))
cat(sprintf("2. anchors identical            = %s   {%s}\n",
            setequal(anchors_topica, anchors_stm),
            paste(sort(vocab[anchors_stm]), collapse = ", ")))

beta_stm <- stm:::recoverL2(Qbar_stm, anchors_stm, Qsums / sum(Qsums), verbose = FALSE)$A
cat("3. recover() vs recoverL2() on identical Qbar + anchors:\n")
for (it in c(120, 500, 5000)) {
  bt <- topica_recover(Qbar_topica, anchors_topica, p_topica, K, V, iters = it)
  cat(sprintf("     topica iters=%-5d  aligned cosine vs stm = %.3f\n", it, cos_align(beta_stm, bt)))
}
cat("\n   -> identical inputs, divergent spectral beta; topica's recovery does not\n",
    "     converge to stm's (cosine is non-monotonic in iters = non-convergent EG).\n")
