#!/usr/bin/env Rscript
# Quick speed check for faSTM: how fit wall-clock scales with corpus size, K,
# and thread count, plus an stm-vs-faSTM speedup reference. Uses the bundled
# gadarian corpus, tiled to larger sizes (we measure throughput, not topic
# quality, so duplicate docs are fine). Iterations are capped (max.em.its) so
# every cell does the same amount of work.
#
#   Rscript speed-bench.R
#
# Knobs (env vars): BENCH_ITS (default 50), BENCH_STM_MAXN (cap on corpus size
# we bother timing stm at, default 1400 — stm is the slow baseline).

suppressMessages({library(stm); library(faSTM)})

ITS      <- as.integer(Sys.getenv("BENCH_ITS", "50"))
STM_MAXN <- as.integer(Sys.getenv("BENCH_STM_MAXN", "1400"))
CORES    <- parallel::detectCores()
cat(sprintf("machine: %d logical cores | max.em.its = %d per fit\n\n", CORES, ITS))

## ---- base corpus (preprocess once) ----------------------------------------
data("gadarian", package = "stm")
proc <- textProcessor(gadarian$open.ended.response, metadata = gadarian, verbose = FALSE)
out  <- prepDocuments(proc$documents, proc$vocab, proc$meta, verbose = FALSE)
base_docs <- out$documents; vocab <- out$vocab; base_meta <- out$meta
N0 <- length(base_docs)

tile <- function(times) {
  idx <- rep(seq_len(N0), times)
  list(docs = base_docs[idx], meta = base_meta[idx, , drop = FALSE])
}

time_fit <- function(corpus, K, threads, engine = "faSTM") {
  f <- if (engine == "faSTM")
    function() faSTM::stm(corpus$docs, vocab, K = K, prevalence = ~ treatment,
                          data = corpus$meta, max.em.its = ITS,
                          num_threads = threads, verbose = FALSE)
  else
    function() stm::stm(corpus$docs, vocab, K = K, prevalence = ~ treatment,
                        data = corpus$meta, max.em.its = ITS, verbose = FALSE)
  as.numeric(system.time(f())[["elapsed"]])
}

rule <- function() cat(strrep("-", 60), "\n")

## ===========================================================================
## Table A — corpus size  (K = 10, threads = 4)
## ===========================================================================
cat("A. Corpus size   (K = 10, threads = 4)\n"); rule()
cat(sprintf("%8s %8s | %10s %10s %9s\n", "docs", "tiling", "faSTM(s)", "stm(s)", "speedup"))
for (m in c(1, 4, 16, 32)) {
  cp <- tile(m); n <- length(cp$docs)
  tf <- time_fit(cp, K = 10, threads = 4, "faSTM")
  if (n <= STM_MAXN) {
    ts <- time_fit(cp, K = 10, threads = 4, "stm")
    cat(sprintf("%8d %7dx | %10.3f %10.3f %8.1fx\n", n, m, tf, ts, ts / tf))
  } else {
    cat(sprintf("%8d %7dx | %10.3f %10s %9s\n", n, m, tf, "(skipped)", "-"))
  }
}

## ===========================================================================
## Table B — number of topics K  (corpus = 4x, threads = 4)
## ===========================================================================
cat("\nB. Topics K   (corpus = 4x ~", N0 * 4, "docs, threads = 4)\n"); rule()
cat(sprintf("%6s | %10s\n", "K", "faSTM(s)"))
cp <- tile(4)
for (K in c(5, 10, 20, 40)) {
  tf <- time_fit(cp, K = K, threads = 4, "faSTM")
  cat(sprintf("%6d | %10.3f\n", K, tf))
}

## ===========================================================================
## Table C — threads  (corpus = 16x, K = 20)
## ===========================================================================
cat("\nC. Threads   (corpus = 16x ~", N0 * 16, "docs, K = 20)\n"); rule()
cat(sprintf("%8s | %10s %9s\n", "threads", "faSTM(s)", "speedup"))
cp <- tile(16)
thread_grid <- Filter(function(x) x <= CORES, c(1, 2, 4, 8))
t1 <- NA
for (th in thread_grid) {
  tf <- time_fit(cp, K = 20, threads = th, "faSTM")
  if (is.na(t1)) t1 <- tf
  cat(sprintf("%8d | %10.3f %8.1fx\n", th, tf, t1 / tf))
}

cat("\nDONE.\n")
