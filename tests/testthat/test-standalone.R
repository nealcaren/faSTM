# Standalone faSTM tests — no stm package required. Need the Rust backend built
# and quanteda for tokenization.

skip_if_not_built <- function() {
  ok <- !is.null(tryCatch(getNativeSymbolInfo("wrap__fit_stm", "faSTM"),
                          error = function(e) NULL))
  if (!ok) skip("faSTM Rust backend not built")
}

make_fit <- function(K = 6L) {
  dfmat <- quanteda::dfm(quanteda::tokens(quanteda::data_corpus_inaugural,
                                          remove_punct = TRUE))
  dfmat <- quanteda::dfm_trim(dfmat, min_termfreq = 5, min_docfreq = 3)
  corpus <- as_corpus(dfmat)
  list(corpus = corpus,
       fit = stm(corpus, K = K, prevalence = ~ Party, seed = 1, verbose = FALSE))
}

test_that("corpus ingestion from a quanteda dfm aligns docs, vocab, meta", {
  skip_if_not_installed("quanteda")
  dfmat <- quanteda::dfm(quanteda::tokens(quanteda::data_corpus_inaugural))
  corpus <- as_corpus(dfmat)
  expect_s3_class(corpus, "faSTM_corpus")
  expect_length(corpus$documents, length(corpus$meta[[1]]))
  expect_equal(length(corpus$vocab), length(corpus$word_counts))
})

test_that("fit returns a usable, stm-shaped object and inspection works", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  expect_s3_class(f$fit, "STM")
  expect_equal(ncol(f$fit$theta), 6L)

  lab <- label_topics(f$fit, n = 5)
  expect_equal(dim(lab$frex), c(6L, 5L))
  expect_true(is.matrix(frex_scores(f$fit)))

  expect_length(semantic_coherence(f$fit), 6L)
  expect_length(exclusivity(f$fit), 6L)
  expect_equal(dim(topic_correlation(f$fit)$cor), c(6L, 6L))
})

test_that("honest estimateEffect yields a coefficient per term per topic", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  eff <- estimateEffect(1:6 ~ Party, f$fit, metadata = f$corpus$meta,
                        nsims = 20L, seed = 1L)
  s <- summary(eff)
  expect_length(s$tables, 6L)
  expect_true("(Intercept)" %in% rownames(s$tables[[1L]]))
})

test_that("svi + covariates is gated until topica STM-SVI is pinned", {
  skip_if_not_built()
  m <- Matrix::Matrix(matrix(c(2, 1, 0, 1, 0, 3), nrow = 2, byrow = TRUE), sparse = TRUE)
  colnames(m) <- c("a", "b", "c")
  expect_error(
    stm(m, K = 2, prevalence = ~ x, data = data.frame(x = c(0, 1)),
        inference = "svi", verbose = FALSE),
    "STM-SVI"
  )
})
