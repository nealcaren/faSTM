# Recovery / compatibility tests. These require the Rust backend to be built
# (rextendr::document() + a pinned topica). They are skipped until then.

skip_if_not_built <- function() {
  if (!is.function(tryCatch(getNativeSymbolInfo("wrap__fit_stm", "faSTM"),
                            error = function(e) NULL))) {
    skip("faSTM Rust backend not built (run rextendr::document())")
  }
}

test_that("fit returns an stm-compatible object", {
  skip_if_not_built()
  skip_if_not_installed("stm")

  data("gadarian", package = "stm")
  processed <- stm::textProcessor(gadarian$open.ended.response, metadata = gadarian,
                                  verbose = FALSE)
  out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta,
                            verbose = FALSE)

  fit <- stm(out$documents, out$vocab, K = 5,
             prevalence = ~ treatment, data = out$meta,
             seed = 1L, verbose = FALSE)

  expect_s3_class(fit, "STM")
  expect_equal(ncol(fit$theta), 5L)
  expect_equal(nrow(fit$theta), length(out$documents))
  # stm's own readers must accept the object
  expect_no_error(stm::labelTopics(fit, n = 5L))
  expect_no_error(stm::findThoughts(fit, texts = gadarian$open.ended.response[
    as.integer(names(out$documents))], n = 1L))
})

test_that("honest estimateEffect produces a coefficient per term and topic", {
  skip_if_not_built()
  skip_if_not_installed("stm")

  data("gadarian", package = "stm")
  processed <- stm::textProcessor(gadarian$open.ended.response, metadata = gadarian,
                                  verbose = FALSE)
  out <- stm::prepDocuments(processed$documents, processed$vocab, processed$meta,
                            verbose = FALSE)
  fit <- stm(out$documents, out$vocab, K = 5, prevalence = ~ treatment,
             data = out$meta, seed = 1L, verbose = FALSE)

  eff <- estimateEffect(1:5 ~ treatment, fit, metadata = out$meta,
                        nsims = 25L, seed = 1L)
  s <- summary(eff)
  expect_length(s$tables, 5L)
  expect_true(all(c("(Intercept)", "treatment") %in% rownames(s$tables[[1L]])))
})

test_that("svi + covariates is gated until topica STM-SVI is pinned", {
  expect_error(
    stm(list(matrix(c(1L, 2L), nrow = 2)), vocab = c("a", "b"), K = 2,
        prevalence = ~ x, data = data.frame(x = 1),
        inference = "svi", verbose = FALSE),
    "STM-SVI"
  )
})
