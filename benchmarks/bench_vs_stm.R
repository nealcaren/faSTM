#!/usr/bin/env Rscript
# Like-for-like faSTM-vs-stm timing on the bundled poliblog data (5000 docs,
# K=20, prevalence ~ rating + s(day)). NOT run in CI; a manual perf check / guard.
# Reports wall-clock for the fit, estimateEffect, and search_k; single- and
# multi-threaded. (A full three-way comparison incl. the Rust `topica` engine
# lives in the topica repo's benchmarks/, since it needs Python.)
#
#   Rscript benchmarks/bench_vs_stm.R
#
# Expected (Apple M-class, mid-2026): fit ~4s threaded / ~15s single vs stm ~23s;
# search_k ~13x. A big jump here flags a regression (e.g. the O(n^2) DTM build
# fixed in 2026-06).

suppressMessages({library(stm); library(faSTM)})
data(poliblog, package = "faSTM")
D <- poliblog$documents; V <- poliblog$vocab; M <- poliblog$meta
tm <- function(e) round(as.numeric(system.time(e)["elapsed"]), 1)
row <- function(lbl, s, f) cat(sprintf("  %-26s stm %5.1fs | faSTM %5.1fs | %.1fx\n", lbl, s, f, s / f))
cat(sprintf("poliblog: %d docs, %d vocab | K=20, prevalence ~ rating + s(day)\n\n",
            length(D), length(V)))

cat("== fit to convergence (emtol=1e-5) ==\n")
ts <- tm({ sf <- stm::stm(D, V, K = 20, prevalence = ~ rating + s(day), data = M,
                          init.type = "Spectral", max.em.its = 500, verbose = FALSE) })
tf  <- tm({ ff <- faSTM::stm(D, vocab = V, K = 20, prevalence = ~ rating + s(day), data = M,
                            init.type = "Spectral", max.em.its = 500, verbose = FALSE) })
tf1 <- tm(faSTM::stm(D, vocab = V, K = 20, prevalence = ~ rating + s(day), data = M,
                     init.type = "Spectral", max.em.its = 500, num_threads = 1, verbose = FALSE))
row("fit (faSTM all cores)", ts, tf)
row("fit (faSTM 1 core)",    ts, tf1)

cat("\n== estimateEffect (matched nsims=25) ==\n")
row("estimateEffect",
    tm(stm::estimateEffect(1:20 ~ rating + s(day), sf, metadata = M, uncertainty = "Global", nsims = 25)),
    tm(faSTM::estimateEffect(1:20 ~ rating + s(day), ff, metadata = M, uncertainty = "Global", nsims = 25)))

cat("\n== search_k over K = c(10, 20) ==\n")
co <- as_corpus(poliblog)
row("search_k",
    tm(stm::searchK(D, V, K = c(10, 20), prevalence = ~ rating + s(day), data = M, verbose = FALSE)),
    tm(faSTM::search_k(co, K = c(10, 20), prevalence = ~ rating + s(day), cores = 2)))
