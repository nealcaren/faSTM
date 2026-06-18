# Drop-in target:
# /Users/nealcaren/Documents/GitHub/faSTM/tests/testthat/test-stm-nongraph-gaps.R
#
# This is a second, stricter STM-compatibility frontier. It intentionally avoids
# graphing and text preprocessing APIs. It assumes documents/vocab are already
# prepared and focuses on non-graphing behavior that common stm code may use but
# faSTM may not fully support yet.

skip_stm_gap_suite <- function() {
  skip_on_cran()
  skip_if_not_installed("stm")
  skip_if_not_installed("Matrix")
}

gap_tiny_input <- function() {
  m <- Matrix::Matrix(
    matrix(
      c(3, 0, 1, 0, 2,
        0, 2, 1, 0, 1,
        1, 0, 0, 4, 0,
        0, 1, 0, 3, 2,
        2, 0, 2, 0, 1,
        0, 3, 0, 1, 0,
        1, 1, 3, 0, 0,
        0, 2, 0, 1, 3),
      nrow = 8,
      byrow = TRUE
    ),
    sparse = TRUE
  )
  colnames(m) <- c("apple", "banana", "carrot", "date", "elder")
  docs <- lapply(seq_len(nrow(m)), function(i) {
    nz <- which(m[i, ] != 0)
    rbind(nz, as.integer(m[i, nz]))
  })
  meta <- data.frame(
    x = seq_len(nrow(m)),
    g = factor(rep(c("a", "b"), length.out = nrow(m))),
    h = factor(rep(c("low", "high"), each = nrow(m) / 2))
  )
  list(documents = docs, vocab = colnames(m), meta = meta, matrix = m)
}

expect_theta_matrix <- function(x, n_docs, k) {
  expect_true(is.matrix(x))
  expect_equal(dim(x), c(n_docs, k))
  expect_true(all(is.finite(x)))
  expect_true(all(abs(rowSums(x) - 1) < 1e-8))
}

test_that("stm fitting init modes beyond Spectral/Random are accepted", {
  skip_stm_gap_suite()
  inp <- gap_tiny_input()

  expect_error(
    stm(inp$documents, inp$vocab, K = 3, init.type = "LDA",
        max.em.its = 2, seed = 1, verbose = FALSE),
    NA
  )

  ref <- stm::stm(inp$documents, inp$vocab, K = 3, init.type = "Random",
                  max.em.its = 0, seed = 1, verbose = FALSE)
  init_beta <- exp(ref$beta$logbeta[[1]])

  expect_error(
    stm(inp$documents, inp$vocab, K = 3, init.type = "Custom",
        model = ref, init.beta = init_beta,
        max.em.its = 0, seed = 1, verbose = FALSE),
    NA
  )
})

test_that("stm fitting accepts common stm-only non-graphing controls", {
  skip_stm_gap_suite()
  inp <- gap_tiny_input()

  expect_error(
    stm(inp$documents, inp$vocab, K = 3,
        init.type = "Random", LDAbeta = TRUE, reportevery = 1,
        control = list(seed = 1), max.em.its = 2, seed = 1, verbose = FALSE),
    NA
  )

  expect_error(
    stm(inp$documents, inp$vocab, K = 3, content = ~ g, data = inp$meta,
        init.type = "Random", interactions = FALSE,
        kappa.prior = "Jeffreys", max.em.its = 2, seed = 1, verbose = FALSE),
    NA
  )
})

test_that("thetaPosterior distinguishes Global and Local semantics", {
  skip_stm_gap_suite()
  inp <- gap_tiny_input()

  fit <- stm(inp$documents, inp$vocab, K = 3, init.type = "Random",
             max.em.its = 2, seed = 1, verbose = FALSE)

  expect_error(thetaPosterior(fit, nsims = 3, type = "Global"), NA)
  expect_error(thetaPosterior(fit, nsims = 3, type = "Local",
                              documents = inp$documents), NA)

  expect_error(
    thetaPosterior(fit, nsims = 3, type = "Local"),
    "Documents must be provided|documents"
  )

  global <- thetaPosterior(fit, nsims = 2, type = "Global")
  local <- thetaPosterior(fit, nsims = 2, type = "Local",
                          documents = inp$documents)
  expect_length(global, 2)
  expect_length(local, 2)
  expect_false(identical(global, local))
})

test_that("estimateEffect supports STM Local uncertainty", {
  skip_stm_gap_suite()
  inp <- gap_tiny_input()

  fit <- stm(inp$documents, inp$vocab, K = 3, prevalence = ~ g + x,
             data = inp$meta, init.type = "Random",
             max.em.its = 2, seed = 1, verbose = FALSE)

  eff <- estimateEffect(1:3 ~ g + x, fit, metadata = inp$meta,
                        uncertainty = "Local", documents = inp$documents,
                        nsims = 3)
  expect_s3_class(eff, "estimateEffect")
  expect_equal(eff$uncertainty, "Local")
})

test_that("fitNewDocuments supports stm prior modes and returnPosterior", {
  skip_stm_gap_suite()
  inp <- gap_tiny_input()

  fit <- stm(inp$documents, inp$vocab, K = 3, prevalence = ~ g + x,
             data = inp$meta, init.type = "Random",
             max.em.its = 2, seed = 1, verbose = FALSE)
  new_docs <- inp$documents[1:3]
  new_data <- inp$meta[1:3, , drop = FALSE]

  avg <- fitNewDocuments(
    model = fit,
    documents = new_docs,
    newData = new_data,
    origData = inp$meta,
    prevalence = ~ g + x,
    prevalencePrior = "Average",
    returnPosterior = FALSE,
    verbose = FALSE
  )
  expect_theta_matrix(avg, length(new_docs), 3)

  cov <- fitNewDocuments(
    model = fit,
    documents = new_docs,
    newData = new_data,
    origData = inp$meta,
    prevalence = ~ g + x,
    prevalencePrior = "Covariate",
    returnPosterior = FALSE,
    verbose = FALSE
  )
  expect_theta_matrix(cov, length(new_docs), 3)
  expect_false(isTRUE(all.equal(avg, cov)))

  posterior <- fitNewDocuments(
    model = fit,
    documents = new_docs,
    newData = new_data,
    origData = inp$meta,
    prevalence = ~ g + x,
    prevalencePrior = "Covariate",
    returnPosterior = TRUE,
    verbose = FALSE
  )
  expect_true(is.list(posterior))
  expect_true(all(c("theta", "eta", "nu") %in% names(posterior)))
  expect_theta_matrix(posterior$theta, length(new_docs), 3)
})

test_that("fitNewDocuments supports content betaIndex/contentPrior semantics", {
  skip_stm_gap_suite()
  inp <- gap_tiny_input()

  fit <- stm(inp$documents, inp$vocab, K = 3, content = ~ g,
             data = inp$meta, init.type = "Random",
             max.em.its = 2, seed = 1, verbose = FALSE)
  new_docs <- inp$documents[1:4]

  a <- fitNewDocuments(
    model = fit,
    documents = new_docs,
    betaIndex = rep(1L, length(new_docs)),
    contentPrior = "Covariate",
    returnPosterior = FALSE,
    verbose = FALSE
  )
  b <- fitNewDocuments(
    model = fit,
    documents = new_docs,
    betaIndex = rep(2L, length(new_docs)),
    contentPrior = "Covariate",
    returnPosterior = FALSE,
    verbose = FALSE
  )

  expect_theta_matrix(a, length(new_docs), 3)
  expect_theta_matrix(b, length(new_docs), 3)
  expect_false(isTRUE(all.equal(a, b)))
})

test_that("stm helper exports used by prepared-corpus workflows exist", {
  skip_stm_gap_suite()

  helpers <- c(
    "alignCorpus",
    "asSTMCorpus",
    "calcfrex",
    "calclift",
    "calcscore",
    "checkBeta",
    "convertCorpus",
    "makeDesignMatrix",
    "optimizeDocument",
    "readLdac",
    "writeLdac"
  )

  missing <- setdiff(helpers, getNamespaceExports("faSTM"))
  if (length(missing) > 0) {
    fail(paste("Missing prepared-corpus helper exports:", paste(missing, collapse = ", ")))
  }
  expect_equal(missing, character(0))
})
