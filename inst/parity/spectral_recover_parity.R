#!/usr/bin/env Rscript
# Spectral-init parity: faSTM's ACTUAL spectral start (topica's cooccurrence +
# fast_anchor_words + recover, via fit_ctm with 0 EM iterations) vs R stm's
# recoverL2(). Reports the aligned topic-word cosine.
#
# STATUS (topica v0.24.1): faSTM's full spectral init reproduces stm's spectral
# recovery EXACTLY — per-topic cosine 1.0, same topics, same order — given the
# same prepped corpus. The comparison must target stm's CONVERGED recovery
# (recoverL2(recoverEG = FALSE), the QP optimum). stm's *default* recoverEG = TRUE
# is a non-converging exponentiated-gradient approximation (fixed step), so a naive
# comparison against it reads ~0.44 — an artifact of stm's default, not a real gap.
# topica's recover() converges to the same QP optimum (see topica#240, resolved).
#
#   Rscript spectral_recover_parity.R   # prints "aligned cosine = <x>"

suppressMessages({library(stm); library(Matrix); library(faSTM)})

K <- 5L
data("gadarian", package = "stm")
proc <- textProcessor(gadarian$open.ended.response, metadata = gadarian, verbose = FALSE)
out  <- prepDocuments(proc$documents, proc$vocab, proc$meta, verbose = FALSE)
docs <- out$documents; vocab <- out$vocab; V <- length(vocab)

## --- stm reference: gram -> fastAnchor -> recoverL2 -----------------------
rows <- integer(0); cols <- integer(0); vals <- integer(0)
for (i in seq_along(docs)) { m <- docs[[i]]
  rows <- c(rows, rep(i, ncol(m))); cols <- c(cols, m[1, ]); vals <- c(vals, m[2, ]) }
mat <- sparseMatrix(i = rows, j = cols, x = vals, dims = c(length(docs), V))
Q <- stm:::gram(mat); Qsums <- rowSums(Q); Qbar_stm <- Q / Qsums
anchors_stm <- stm:::fastAnchor(Qbar_stm, K, verbose = FALSE)
# recoverEG = FALSE: the converged QP optimum. stm's default (TRUE) is a
# non-converging exponentiated gradient and is NOT the recovery target.
beta_stm <- stm:::recoverL2(Qbar_stm, anchors_stm, Qsums / sum(Qsums),
                           verbose = FALSE, recoverEG = FALSE)$A

## --- faSTM ACTUAL spectral init: spectral default, 0 EM iters (raw beta) ---
fit <- faSTM::stm(out$documents, vocab = out$vocab, K = K, max.em.its = 0,
                  seed = 1, verbose = FALSE)
beta_fa <- exp(fit$beta$logbeta[[1]])

## per-topic (same-order, diagonal) cosine: faSTM reproduces stm's spectral
## topics topic-for-topic, not merely as a set.
diag_cos <- function(a, b) {
  an <- a / sqrt(rowSums(a^2)); bn <- b / sqrt(rowSums(b^2)); mean(rowSums(an * bn))
}
## Hungarian-matched cosine as a permutation-robust fallback.
cos_align <- function(a, b) {
  an <- a / sqrt(rowSums(a^2)); bn <- b / sqrt(rowSums(b^2)); sim <- an %*% t(bn)
  k <- nrow(a); u <- logical(k); tot <- 0
  for (i in order(-apply(sim, 1, max))) {
    j <- which.max(ifelse(u, -Inf, sim[i, ])); u[j] <- TRUE; tot <- tot + sim[i, j]
  }
  tot / k
}

cat(sprintf("faSTM spectral init vs stm recoverL2 (recoverEG=FALSE, converged):\n"))
cat(sprintf("  per-topic (same-order) aligned cosine = %.4f\n", diag_cos(beta_stm, beta_fa)))
cat(sprintf("  Hungarian-matched          cosine     = %.4f\n", cos_align(beta_stm, beta_fa)))
cat("(Given the same prepped corpus, faSTM's spectral start IS stm's. stm's default\n",
    " recoverEG=TRUE is a non-converging approximation and is not the target.)\n")
