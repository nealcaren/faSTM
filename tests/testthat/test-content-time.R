test_that("content_time crosses into saturated period cells and smooths them", {
  skip_on_cran()
  set.seed(1)
  # tiny corpus: 2 groups x 3 periods, a marker word whose group contrast drifts
  V <- 8L; np <- 3L
  make_doc <- function(g, t) {
    base <- sample.int(V, 6, replace = TRUE)
    marker <- if (g == "A") rep(7L, t) else rep(8L, np - t + 1)  # drift by period
    tab <- tabulate(c(base, marker), nbins = V)
    rbind(which(tab > 0), tab[tab > 0])
  }
  docs <- list(); meta <- data.frame(grp = character(), yr = integer())
  for (g in c("A", "B")) for (t in 1:np) for (r in 1:12) {
    docs[[length(docs) + 1L]] <- matrix(as.integer(make_doc(g, t)), nrow = 2)
    meta <- rbind(meta, data.frame(grp = g, yr = 2000L + t))
  }
  vocab <- as.character(seq_len(V))

  # (a) no content_time: ordinary content model, 2 groups
  m0 <- stm(docs, vocab, K = 3L, content = ~ grp, data = meta,
            init.type = "Spectral", seed = 1L, verbose = FALSE)
  expect_equal(m0$settings$dim$A, 2L)

  # (b) content_time: saturated 2 x 3 = 6 cells
  fit <- function(s) stm(docs, vocab, K = 3L, content = ~ grp, content_time = ~ yr,
                         content_smooth = s, data = meta, init.type = "Spectral",
                         seed = 1L, verbose = FALSE)
  m_sat <- fit(0.0); m_smo <- fit(5.0)
  expect_equal(m_sat$settings$dim$A, 6L)
  expect_equal(m_smo$settings$dim$A, 6L)

  # smoothing shrinks adjacent-period content differences
  adj_gap <- function(m) {
    lb <- m$beta$logbeta; acc <- 0; n <- 0
    for (b in 0:1) for (t in 1:(np - 1)) {
      g2 <- b * np + t + 1; g1 <- b * np + (t - 1) + 1
      d <- exp(lb[[g2]]) - exp(lb[[g1]]); acc <- acc + sum(d * d); n <- n + length(d)
    }
    acc / n
  }
  expect_lt(adj_gap(m_smo), adj_gap(m_sat))

  # error on a single period (nothing to smooth)
  meta1 <- meta; meta1$yr <- 2001L
  expect_error(stm(docs, vocab, K = 3L, content_time = ~ yr, data = meta1,
                   verbose = FALSE), "< 2 periods")
})

test_that("stm() validates content-prior scales in R (clear error, not a Rust panic)", {
  skip_on_cran()
  set.seed(1)
  V <- 6L
  docs <- lapply(1:12, function(i) {
    tab <- tabulate(sample.int(V, 5, replace = TRUE), nbins = V)
    matrix(as.integer(rbind(which(tab > 0), tab[tab > 0])), nrow = 2)
  })
  vocab <- as.character(seq_len(V))
  meta <- data.frame(grp = rep(c("A", "B"), length.out = 12),
                     yr = rep(2000:2001, each = 6))
  base <- function(...) stm(docs, vocab, K = 2L, content = ~ grp, data = meta,
                            init.type = "Spectral", seed = 1L, verbose = FALSE, ...)
  expect_error(base(content_prior = "l1", content_prior_var = 0), "content_prior_var")
  expect_error(base(content_prior_var = -1), "content_prior_var")
  expect_error(base(content_prior_var = Inf), "content_prior_var")
  expect_error(base(content_time = ~ yr, content_smooth = -1), "content_smooth")
  expect_error(base(content_time = ~ yr, content_smooth = NA_real_), "content_smooth")
})
