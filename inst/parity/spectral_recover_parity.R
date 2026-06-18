#!/usr/bin/env Rscript
# Spectral-init parity: faSTM's ACTUAL spectral start (topica's cooccurrence +
# fast_anchor_words + recover, via fit_ctm with 0 EM iterations) vs R stm's
# recoverL2(). Reports the aligned topic-word cosine.
#
# STATUS (topica v0.24.1): topica's recover() STEP reproduces recoverL2() at
# cosine 1.0 in isolation (topica parity/spectral_recover_stm.py). But the FULL
# spectral_init pipeline diverges end-to-end (aligned cosine ~0.44) — the gap is
# in topica's own cooccurrence()/anchor selection, which that parity substitutes
# with stm's and so never tests. Tracked: topica#240.
#
# This is NOT a correctness issue: both spectral inits are valid starts that EM
# refines. For a guaranteed replication of a specific stm fit, start faSTM from
# stm's own spectral beta: stm(..., init.beta = <K x V>) (round-trips at cosine 1).
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
beta_stm <- stm:::recoverL2(Qbar_stm, anchors_stm, Qsums / sum(Qsums), verbose = FALSE)$A

## --- faSTM ACTUAL spectral init: spectral default, 0 EM iters (raw beta) ---
fit <- faSTM::stm(out$documents, vocab = out$vocab, K = K, max.em.its = 0,
                  seed = 1, verbose = FALSE)
beta_fa <- exp(fit$beta$logbeta[[1]])

## aligned cosine (greedy topic matching)
cos_align <- function(a, b) {
  an <- a / sqrt(rowSums(a^2)); bn <- b / sqrt(rowSums(b^2)); sim <- an %*% t(bn)
  k <- nrow(a); u <- logical(k); tot <- 0
  for (i in order(-apply(sim, 1, max))) {
    j <- which.max(ifelse(u, -Inf, sim[i, ])); u[j] <- TRUE; tot <- tot + sim[i, j]
  }
  tot / k
}

cat(sprintf("faSTM spectral init vs stm recoverL2: aligned cosine = %.4f\n",
            cos_align(beta_stm, beta_fa)))
cat("(topica#240: full spectral_init parity; recover() step alone matches at 1.0.\n",
    " For exact replication use stm(..., init.beta = stm's spectral beta).)\n")
