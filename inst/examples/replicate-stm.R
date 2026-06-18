#!/usr/bin/env Rscript
# Replicate an stm analysis with faSTM and compare, end to end.
#
#   stm::textProcessor + prepDocuments   (shared input)
#   stm::stm()      vs   faSTM::stm()    (the swap)
#   stm::estimateEffect vs faSTM::estimateEffect
#
# Topics are unidentified up to permutation, so we align faSTM's topics to
# stm's by greedy correlation of the topic-word distributions, then report how
# closely the two agree on (a) topic-word content, (b) document-topic
# proportions, and (c) the treatment effect on prevalence.

suppressMessages({
  library(stm)
  library(faSTM)
})

set.seed(1)
K    <- 5L
SEED <- 1L

## ---- shared preprocessing (stm) -------------------------------------------
data("gadarian", package = "stm")
proc <- textProcessor(gadarian$open.ended.response, metadata = gadarian,
                      verbose = FALSE)
out  <- prepDocuments(proc$documents, proc$vocab, proc$meta, verbose = FALSE)
docs <- out$documents; vocab <- out$vocab; meta <- out$meta
cat(sprintf("corpus: %d docs, %d vocab\n", length(docs), length(vocab)))

## ---- fit both --------------------------------------------------------------
cat("\n== fitting stm::stm ==\n")
t_stm <- system.time(
  m_stm <- stm::stm(docs, vocab, K = K, prevalence = ~ treatment,
                    data = meta, init.type = "Spectral",
                    seed = SEED, verbose = FALSE)
)

cat("== fitting faSTM::stm ==\n")
t_fa <- system.time(
  m_fa <- faSTM::stm(docs, vocab, K = K, prevalence = ~ treatment,
                     data = meta, init.type = "Spectral",
                     seed = SEED, verbose = FALSE)
)
cat(sprintf("\nwall-clock: stm %.2fs   faSTM %.2fs\n",
            t_stm[["elapsed"]], t_fa[["elapsed"]]))

## ---- align faSTM topics to stm topics (greedy on beta correlation) --------
beta_stm <- exp(m_stm$beta$logbeta[[1]])      # K x V
beta_fa  <- exp(m_fa$beta$logbeta[[1]])
cmat <- cor(t(beta_stm), t(beta_fa))          # stm row i vs faSTM col j
perm <- integer(K); used <- logical(K)
for (i in order(-apply(cmat, 1, max))) {
  j <- which.max(ifelse(used, -Inf, cmat[i, ]))
  perm[i] <- j; used[j] <- TRUE
}
cat("\ntopic alignment (stm -> faSTM):", paste(seq_len(K), "->", perm), "\n")
cat("aligned topic-word correlations:",
    sprintf("%.3f", diag(cmat[, perm])), "\n")

## ---- compare top words -----------------------------------------------------
top_words <- function(beta, vocab, n = 8) apply(beta, 1, function(b)
  paste(vocab[order(-b)][seq_len(n)], collapse = " "))
tw_stm <- top_words(beta_stm, vocab)
tw_fa  <- top_words(beta_fa[perm, , drop = FALSE], vocab)
cat("\n== top words per topic ==\n")
for (k in seq_len(K)) {
  cat(sprintf("T%d  stm  : %s\n", k, tw_stm[k]))
  cat(sprintf("    faSTM: %s\n", tw_fa[k]))
}

## ---- compare document-topic proportions -----------------------------------
theta_cor <- diag(cor(m_stm$theta, m_fa$theta[, perm, drop = FALSE]))
cat(sprintf("\ntheta (doc-topic) correlations per topic: %s\n",
            paste(sprintf("%.3f", theta_cor), collapse = " ")))
cat(sprintf("mean theta correlation: %.3f\n", mean(theta_cor)))

## ---- compare the treatment effect -----------------------------------------
ee_stm <- stm::estimateEffect(1:K ~ treatment, m_stm, metadata = meta,
                              uncertainty = "Global")
s_stm  <- summary(ee_stm)
ee_fa  <- faSTM::estimateEffect(1:K ~ treatment, m_fa, metadata = meta,
                                uncertainty = "Global", nsims = 100L, seed = 1L)
s_fa   <- summary(ee_fa)

get_treat <- function(tab) tab["treatment", "Estimate"]
eff_stm <- vapply(s_stm$tables, get_treat, numeric(1))             # by stm topic
eff_fa  <- vapply(s_fa$tables[perm], function(t) t["treatment", "Estimate"],
                  numeric(1))
cat("\n== treatment effect on topic prevalence (aligned) ==\n")
print(round(data.frame(topic = seq_len(K), stm = eff_stm, faSTM = eff_fa,
                       diff = eff_stm - eff_fa), 4), row.names = FALSE)
cat(sprintf("\ncorrelation of treatment effects across topics: %.3f\n",
            cor(eff_stm, eff_fa)))

cat("\nDONE.\n")
