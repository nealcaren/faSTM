# Readers over the content_time surface: content_trajectory() / content_divergence()
# (point estimates + the bootstrap-CI machinery, incl. the all-refits-failed guard).

# Build a tiny 2-group x 3-period content_time corpus with a drifting marker word.
.ct_corpus <- function() {
  set.seed(1)
  V <- 10L; np <- 3L; vocab <- as.character(seq_len(V))
  make_doc <- function(g, t) {
    base <- sample.int(8L, 8L, replace = TRUE)               # words 1..8 shared filler
    marker <- if (g == "A") rep(9L, t + 1L) else rep(10L, np - t + 2L)
    tab <- tabulate(c(base, marker), nbins = V)
    matrix(as.integer(rbind(which(tab > 0), tab[tab > 0])), nrow = 2)
  }
  docs <- list(); meta <- data.frame(grp = character(), yr = integer())
  for (g in c("A", "B")) for (t in seq_len(np)) for (r in 1:14) {
    docs[[length(docs) + 1L]] <- make_doc(g, t)
    meta <- rbind(meta, data.frame(grp = g, yr = 2000L + t))
  }
  names(docs) <- paste0("d", seq_along(docs))
  list(docs = docs, vocab = vocab, meta = meta, np = np)
}

test_that("content_trajectory / content_divergence read the content_time surface", {
  skip_on_cran()
  cc <- .ct_corpus()
  fit <- stm(cc$docs, cc$vocab, K = 2L, content = ~ grp, content_time = ~ yr,
             data = cc$meta, init.type = "Spectral", seed = 1L, verbose = FALSE)

  # per-word trajectory: one row per (word x period), finite estimates
  tr <- content_trajectory(fit, words = c("9", "10"), groups = c("A", "B"), topic = 1L)
  expect_setequal(colnames(tr), c("word", "period", "estimate"))
  expect_equal(nrow(tr), 2L * cc$np)
  expect_true(all(is.finite(tr$estimate)))
  # periods are read in chronological (numeric) order
  expect_equal(unique(tr$period), c("2001", "2002", "2003"))

  # divergence: one non-negative value per period
  dv <- content_divergence(fit, groups = c("A", "B"), topic = 1L)
  expect_setequal(colnames(dv), c("period", "divergence"))
  expect_equal(nrow(dv), cc$np)
  expect_true(all(dv$divergence >= 0 | is.na(dv$divergence)))

  # a non-content_time model has no group@period surface to read
  m_plain <- stm(cc$docs, cc$vocab, K = 2L, content = ~ grp, data = cc$meta,
                 init.type = "Spectral", seed = 1L, verbose = FALSE)
  expect_error(content_trajectory(m_plain, words = "9", groups = c("A", "B"), topic = 1L),
               "content_time")
})

test_that("bootstrap CI errors clearly when every refit fails", {
  skip_on_cran()
  cc <- .ct_corpus()
  corp <- structure(list(documents = cc$docs, vocab = cc$vocab, meta = cc$meta),
                    class = "faSTM_corpus")
  fit <- stm(cc$docs, cc$vocab, K = 2L, content = ~ grp, content_time = ~ yr,
             data = cc$meta, init.type = "Spectral", seed = 1L, verbose = FALSE)
  anchor <- c("9", "10")
  # fit_args that reference a nonexistent covariate make every refit throw, so the
  # bootstrap collects zero replicates. Previously cbind(<nothing>) -> a cryptic
  # apply(NULL, ...) error; now it must stop with a clear message.
  bad <- list(K = 2L, content = ~ grp, content_time = ~ not_a_column)
  expect_error(
    content_trajectory(fit, words = "9", groups = c("A", "B"), anchor_words = anchor,
                       ci = TRUE, corpus = corp, fit_args = bad, B = 3L, seed = 1L),
    "no usable replicates")
  expect_error(
    content_divergence(fit, groups = c("A", "B"), anchor_words = anchor,
                       ci = TRUE, corpus = corp, fit_args = bad, B = 3L, seed = 1L),
    "no usable replicates")
})

test_that("periods follow the fit's ordered-factor level order, not an alpha sort", {
  skip_on_cran()
  cc <- .ct_corpus()
  # relabel the three years as a deliberately non-alphabetical ordered factor:
  # level order pre < during < post, whose sort() order (during, post, pre) differs.
  phase <- factor(c("pre", "during", "post")[cc$meta$yr - 2000L],
                  levels = c("pre", "during", "post"))
  meta <- data.frame(grp = cc$meta$grp, phase = phase)
  fit <- stm(cc$docs, cc$vocab, K = 2L, content = ~ grp, content_time = ~ phase,
             data = meta, init.type = "Spectral", seed = 1L, verbose = FALSE)

  tr <- content_trajectory(fit, words = c("9", "10"), groups = c("A", "B"), topic = 1L)
  # chronological (factor-level) order, NOT sort() -> would be during, post, pre
  expect_equal(unique(tr$period), c("pre", "during", "post"))
  dv <- content_divergence(fit, groups = c("A", "B"), topic = 1L)
  expect_equal(dv$period, c("pre", "during", "post"))
})

test_that("readers guard degenerate inputs (no vocab match, single content group)", {
  skip_on_cran()
  cc <- .ct_corpus()
  fit <- stm(cc$docs, cc$vocab, K = 2L, content = ~ grp, content_time = ~ yr,
             data = cc$meta, init.type = "Spectral", seed = 1L, verbose = FALSE)
  # no requested word is in the vocabulary -> clear error, not a NULL/malformed frame
  expect_error(
    content_trajectory(fit, words = "not_a_word", groups = c("A", "B"), topic = 1L),
    "vocabulary")

  # content_time without a content covariate has a single base group, so the default
  # two-group contrast is undefined -> error rather than silent all-NA output
  fit1 <- stm(cc$docs, cc$vocab, K = 2L, content_time = ~ yr, data = cc$meta,
              init.type = "Spectral", seed = 1L, verbose = FALSE)
  expect_error(content_divergence(fit1, topic = 1L), "single content group")

  # an explicit `groups` must name exactly two groups that exist in the model
  expect_error(content_divergence(fit, groups = "A", topic = 1L),
               "exactly two")
  expect_error(content_divergence(fit, groups = c("A", "ZZ"), topic = 1L),
               "not in the model")
})

test_that("bootstrap drops a whole replicate when a refit reader throws", {
  skip_on_cran()
  cc <- .ct_corpus()
  corp <- structure(list(documents = cc$docs, vocab = cc$vocab, meta = cc$meta),
                    class = "faSTM_corpus")
  fit <- stm(cc$docs, cc$vocab, K = 2L, content = ~ grp, content_time = ~ yr,
             data = cc$meta, init.type = "Spectral", seed = 1L, verbose = FALSE)
  # fit_args that OMIT content_time: each refit *succeeds* but the reader then throws
  # ("not fit with a content_time covariate"). That per-replicate error must be caught
  # and the replicate dropped, surfacing the clean all-failed guard -- not crash.
  bad <- list(K = 2L, content = ~ grp, init.type = "Spectral", seed = 1L)
  expect_error(
    content_divergence(fit, groups = c("A", "B"), anchor_words = c("9", "10"),
                       ci = TRUE, corpus = corp, fit_args = bad, B = 3L, seed = 1L),
    "no usable replicates")
})

test_that("bootstrap CI attaches percentile bands on the happy path", {
  skip_on_cran()
  cc <- .ct_corpus()
  corp <- structure(list(documents = cc$docs, vocab = cc$vocab, meta = cc$meta),
                    class = "faSTM_corpus")
  fit <- stm(cc$docs, cc$vocab, K = 2L, content = ~ grp, content_time = ~ yr,
             data = cc$meta, init.type = "Spectral", seed = 1L, verbose = FALSE)
  args <- list(K = 2L, content = ~ grp, content_time = ~ yr,
               init.type = "Spectral", seed = 1L)
  dv <- content_divergence(fit, groups = c("A", "B"), anchor_words = c("9", "10"),
                           ci = TRUE, corpus = corp, fit_args = args, B = 3L, seed = 1L)
  expect_true(all(c("conf.low", "conf.high") %in% colnames(dv)))
  expect_true(all(dv$conf.low <= dv$conf.high | is.na(dv$conf.low)))
})
