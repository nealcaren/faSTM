# Standalone faSTM tests — no stm package required. Need the Rust backend built
# and quanteda for tokenization.

skip_if_not_built <- function() {
  ok <- !is.null(tryCatch(getNativeSymbolInfo("wrap__fit_stm", "faSTM"),
                          error = function(e) NULL))
  if (!ok) skip("faSTM Rust backend not built")
}

test_that("documents->DTM build is linear (guards the O(n^2) regression)", {
  skip_on_cran()
  mk <- function(n) lapply(seq_len(n), function(i)
    matrix(c(sample.int(80L, 8L), rep(2L, 8L)), nrow = 2L, byrow = TRUE))
  # 8000 docs: the old c()-in-a-loop build took seconds; the linear build is ~instant.
  t <- system.time(faSTM:::.documents_to_dtm(mk(8000L), 80L))[["elapsed"]]
  expect_lt(t, 2)   # quadratic would be many seconds even on a slow runner
})

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

test_that("search_k returns diagnostics per K and a tidy long form", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  dfmat <- quanteda::dfm_trim(
    quanteda::dfm(quanteda::tokens(quanteda::data_corpus_inaugural, remove_punct = TRUE)),
    min_termfreq = 5, min_docfreq = 3)
  corpus <- as_corpus(dfmat)
  res <- search_k(corpus, K = c(5L, 8L), prevalence = ~ Party, cores = 1L, seed = 1L)
  expect_s3_class(res, "faSTM_searchk")
  expect_equal(nrow(res$results), 2L)
  expect_true(all(c("K", "heldout", "semcoh", "exclusivity", "bound") %in% names(res$results)))
  expect_true(all(is.finite(res$results$heldout)))
  long <- as.data.frame(res)
  expect_true(all(c("K", "metric", "value") %in% names(long)))
})

test_that("select_model returns a frontier and select_best picks one fit", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  sel <- select_model(f$corpus, K = 6, N = 3, prevalence = ~ Party, cores = 1, seed = 1)
  expect_s3_class(sel, "faSTM_selectmodel")
  expect_length(sel$models, 3L)
  expect_true(length(sel$frontier) >= 1L)
  expect_s3_class(select_best(sel), "STM")
})

test_that("conveniences: s(), check_residuals, make_dt, find_topic, sage_labels", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  expect_true(is.matrix(s(1:20)))
  cr <- check_residuals(f$fit)
  expect_true(is.finite(cr$dispersion))
  dt <- make_dt(f$fit)
  expect_equal(ncol(dt), 7L)                    # document + 6 topics
  expect_type(find_topic(f$fit, "nation"), "integer")

  fc <- stm(f$corpus, K = 4, content = ~ Party, seed = 1, verbose = FALSE)
  sl <- sage_labels(fc, n = 4)
  expect_s3_class(sl, "faSTM_sagelabels")
  expect_equal(length(sl$groups), nlevels(factor(f$corpus$meta$Party)))
})

test_that("fit_new_documents infers topics out of sample", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  th <- fit_new_documents(f$fit, f$corpus)         # self-inference
  expect_equal(dim(th), dim(f$fit$theta))
  expect_true(all(abs(rowSums(th) - 1) < 1e-8))
  # reproduces the fitted proportions closely
  cosines <- vapply(seq_len(nrow(th)), function(i)
    sum(th[i, ] * f$fit$theta[i, ]) /
      sqrt(sum(th[i, ]^2) * sum(f$fit$theta[i, ]^2)), numeric(1))
  expect_gt(mean(cosines), 0.99)
})

test_that("ggplot2 plot methods return ggplot objects", {
  skip_if_not_built(); skip_if_not_installed("quanteda"); skip_if_not_installed("ggplot2")
  f <- make_fit(6L)
  expect_s3_class(plot(f$fit, type = "summary"), "ggplot")
  expect_s3_class(plot_topic_network(f$fit), "ggplot")

  sk <- search_k(f$corpus, K = c(5L, 8L), prevalence = ~ Party, cores = 1L, seed = 1L)
  expect_s3_class(plot(sk), "ggplot")

  eff <- estimateEffect(1:6 ~ Party, f$fit, metadata = f$corpus$meta, nsims = 20L, seed = 1L)
  expect_s3_class(plot(eff, "Party", method = "pointestimate"), "ggplot")
})

test_that("niche: ldac round-trips, multi_stm + permutation_test run", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(5L)
  # ldac round-trip
  tmp <- tempfile(fileext = ".ldac")
  write_ldac(f$corpus$documents[1:5], tmp)
  expect_equal(unname(read_ldac(tmp)), unname(f$corpus$documents[1:5]))
  # multi_stm from a small select_model
  sel <- select_model(f$corpus, K = 5, N = 2, cores = 1, seed = 1)
  ms <- multi_stm(sel, n = 10)
  expect_s3_class(ms, "faSTM_multistm")
  expect_length(ms$stability, 5L)
  # permutation test (tiny; balanced binary treatment)
  cc <- f$corpus
  cc$meta$post <- as.integer(cc$meta$Year >= stats::median(cc$meta$Year))
  pt <- permutation_test(1:5 ~ post, stm(cc, K = 5, prevalence = ~ post, seed = 1, verbose = FALSE),
                         "post", cc, nruns = 3, seed = 1)
  expect_s3_class(pt, "faSTM_permtest")
  expect_equal(dim(pt$null), c(2L, 5L))
})

test_that("stm-compat aliases and new plot types work", {
  skip_if_not_built(); skip_if_not_installed("quanteda"); skip_if_not_installed("ggplot2")
  f <- make_fit(6L)
  # aliases accepting (documents, vocab)
  sk <- searchK(f$corpus$documents, f$corpus$vocab, K = c(5L, 8L),
                data = f$corpus$meta, prevalence = ~ Party, cores = 1L)
  expect_s3_class(sk, "faSTM_searchk")
  # labelTopics subset + findThoughts
  lt <- labelTopics(f$fit, topics = c(1L, 3L), n = 4L)
  expect_equal(nrow(lt$frex), 2L)
  expect_no_error(print(lt))
  # topicCorr + its plot, perspectives, plotModels
  expect_s3_class(plot(topicCorr(f$fit)), "ggplot")
  expect_s3_class(plot(f$fit, type = "perspectives", topics = c(1L, 3L)), "ggplot")
  expect_s3_class(plot(f$fit, type = "hist"), "ggplot")
  sel <- selectModel(f$corpus$documents, f$corpus$vocab, K = 6, N = 2, seed = 1)
  expect_s3_class(plotModels(sel), "ggplot")
  # estimateEffect via meta= + difference plot with stm-style cov.value args
  eff <- estimateEffect(1:6 ~ Party, f$fit, meta = f$corpus$meta, nsims = 20L, seed = 1L)
  expect_s3_class(plot(eff, "Party", method = "difference",
                       cov.value1 = "Republican", cov.value2 = "Democratic"), "ggplot")
})

test_that("difference plot with a spline term reuses fitted knots (makepredictcall.s)", {
  skip_if_not_built(); skip_if_not_installed("quanteda"); skip_if_not_installed("ggplot2")
  f <- make_fit(6L)
  f$corpus$meta$yr <- as.numeric(f$corpus$meta$Year)
  fit <- stm(f$corpus, K = 6, prevalence = ~ Party + s(yr), data = f$corpus$meta,
             seed = 1, verbose = FALSE)
  eff <- estimateEffect(1:6 ~ Party + s(yr), fit, metadata = f$corpus$meta,
                        nsims = 20L, seed = 1L)
  # holds the spline var constant -> the basis must rebuild to the fitted df,
  # not collapse to degree (the bug that broke the vignette difference plot).
  p <- plot(eff, "Party", method = "difference",
            cov.value1 = "Republican", cov.value2 = "Democratic", topics = 1:2)
  expect_s3_class(p, "ggplot")
})

test_that("random effects in estimateEffect (#253)", {
  skip_if_not_built(); skip_if_not_installed("quanteda"); skip_if_not_installed("lme4")
  f <- make_fit(5L); m <- f$corpus$meta
  set.seed(1); m$grp <- factor(sample(paste0("g", 1:6), nrow(m), TRUE))
  ef <- estimateEffect(1:5 ~ Party + (1 | grp), f$fit, metadata = m, nsims = 15L, seed = 1L)
  expect_true(isTRUE(ef$random))
  expect_true("(Intercept)" %in% ef$terms)
  expect_false(is.null(ef$varcomp))
  expect_true("grp" %in% ef$varcomp[["topic1"]]$grp)         # variance component recovered
  s <- summary(ef, topics = 1)
  expect_true(isTRUE(s$random))
  expect_true(all(c("Estimate", "Std. Error") %in% names(s$tables[[1]])))
  # fixed-effects machinery (tidy / ame) still works on a mixed-model fit
  expect_true("estimate" %in% names(generics::tidy(ef)))
  expect_no_error(ame(ef, "Party", topics = 1))
})

test_that("medium gains: weights + cluster SEs, AME, coherence, broom, predict", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L); m <- f$corpus$meta
  m$wt <- seq(0.5, 2, length.out = nrow(m)); m$cl <- factor(m$Party)
  # weights change estimates; cluster changes SEs
  e0 <- estimateEffect(1:6 ~ Party, f$fit, metadata = m, nsims = 15L, seed = 1L)
  ew <- estimateEffect(1:6 ~ Party, f$fit, metadata = m, nsims = 15L, seed = 1L, weights = m$wt)
  ec <- estimateEffect(1:6 ~ Party, f$fit, metadata = m, nsims = 15L, seed = 1L, cluster = m$cl)
  expect_false(isTRUE(all.equal(e0$coefficients[[1]]$est, ew$coefficients[[1]]$est)))
  expect_false(isTRUE(all.equal(e0$coefficients[[1]]$se,  ec$coefficients[[1]]$se)))
  # AME
  a <- ame(e0, "Party", topics = 1:2)
  expect_true(all(c("topic", "term", "ame", "se", "lower", "upper") %in% names(a)))
  # coherence variants finite, one per topic
  for (mz in c("mimno", "npmi", "c_v")) expect_length(coherence(f$fit, mz), 6L)
  expect_true(all(is.finite(coherence(f$fit, "npmi"))))
  # broom tidiers + predict
  skip_if_not_installed("generics")
  expect_named(generics::tidy(f$fit), c("topic", "term", "beta"))
  expect_equal(nrow(generics::tidy(f$fit, matrix = "gamma")), nrow(f$fit$theta) * 6L)
  expect_equal(generics::glance(f$fit)$k, 6L)
  expect_named(generics::augment(f$fit), c("document", "term", "count", ".topic"))
  expect_true("estimate" %in% names(generics::tidy(e0)))
  expect_equal(dim(predict(f$fit, f$corpus)), dim(f$fit$theta))
})

test_that("stm-wishlist: topic_proportions (#269) + make_dt meta guard (#247)", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  tp <- topic_proportions(f$fit)
  expect_true(all(c("topic", "proportion", "label") %in% names(tp)))
  expect_equal(sum(tp$proportion), 1, tolerance = 1e-8)
  expect_true(all(diff(tp$proportion) <= 0))           # sorted descending
  expect_silent(make_dt(f$fit, f$corpus$meta))
  expect_error(make_dt(f$fit, f$corpus$meta[1:3, ]), "rows but the model")
})

test_that("stm-wishlist: sage_labels frexweight + estimateEffect combine topics", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  # #189: frexweight changes SAGE group labels
  fc <- stm(f$corpus, K = 4, content = ~ Party, data = f$corpus$meta, seed = 1, verbose = FALSE)
  expect_false(identical(sage_labels(fc, n = 4)$bygroup,
                         sage_labels(fc, n = 4, frexweight = 0.5)$bygroup))
  # #200: combine topics into an aggregate effect
  eff <- estimateEffect(1:6 ~ Party, f$fit, metadata = f$corpus$meta, nsims = 20L, seed = 1L,
                        combine = list(grp = c(1L, 3L)))
  expect_true("grp" %in% names(eff$coefficients))
  expect_true("grp" %in% names(summary(eff)$tables))
})

test_that("stm-wishlist: effect_estimates data extractor + topic_corr_graph igraph", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  eff <- estimateEffect(1:6 ~ Party, f$fit, metadata = f$corpus$meta, nsims = 20L, seed = 1L)
  # #83: get the plot data without plotting
  ee <- effect_estimates(eff, "Party", method = "pointestimate", topics = c(1L, 2L))
  expect_true(all(c("topic", "value", "est", "se", "lower", "upper") %in% names(ee)))
  expect_true(all(ee$lower <= ee$est & ee$est <= ee$upper))
  # #242: topicCorr as an igraph graph
  skip_if_not_installed("igraph")
  g <- topic_corr_graph(f$fit)
  expect_true(igraph::is_igraph(g))
  expect_equal(igraph::vcount(g), 6L)
  expect_true("prevalence" %in% igraph::vertex_attr_names(g))
})

test_that("stm-wishlist extras: numeric FREX values, effect R^2/F, p.adjust", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  # #265: topic_terms returns the numeric score behind each top word
  tt <- topic_terms(f$fit, n = 4, by = "frex")
  expect_true(all(c("topic", "rank", "term", "score", "measure") %in% names(tt)))
  expect_equal(nrow(tt), 6L * 4L)
  expect_true(is.numeric(tt$score))
  # #255 + #224: estimateEffect diagnostics and p-value adjustment
  eff <- estimateEffect(1:6 ~ Party, f$fit, metadata = f$corpus$meta, nsims = 20L, seed = 1L)
  s <- summary(eff, p.adjust.method = "BH")
  expect_true(all(c("r.squared", "fstatistic", "df.num", "df.den") %in% names(s$diagnostics)))
  expect_equal(s$p.adjust.method, "BH")
  raw <- summary(eff)$tables[[1]][["Pr(>|t|)"]]
  adj <- s$tables[[1]][["Pr(>|t|)"]]
  expect_true(all(adj >= raw - 1e-12))                # BH never decreases p-values
})

test_that("init.type='LDA' seeds from a real CVB0 LDA (no warning, distinct topics)", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  w <- NULL
  m0 <- withCallingHandlers(
    stm(f$corpus, K = 6, init.type = "LDA", max.em.its = 0, seed = 1, verbose = FALSE),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  expect_null(w)                                   # real init, not a warned fallback
  B <- exp(m0$beta$logbeta[[1]])                   # 0 EM -> raw LDA-init beta
  expect_true(all(abs(rowSums(B) - 1) < 1e-6))     # valid topic-word distributions
  expect_equal(nrow(unique(round(B, 4))), 6L)      # K distinct topics
})

test_that("init.beta starts the fit from a supplied initialization", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(6L)
  B <- exp(f$fit$beta$logbeta[[1]])              # a valid K x V topic-word matrix
  m0 <- stm(f$corpus, K = 6, init.beta = B, max.em.its = 0, seed = 1, verbose = FALSE)
  # 0 EM iterations from B returns B
  expect_equal(exp(m0$beta$logbeta[[1]]), B, tolerance = 1e-8)
})

test_that("multiple content covariates fit a crossed SAGE model + marginal recovery", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(5L)
  f$corpus$meta$era <- factor(ifelse(f$corpus$meta$Year < 1900, "pre", "post"))
  np <- nlevels(factor(f$corpus$meta$Party)); ne <- 2L
  fc <- stm(f$corpus, K = 4, content = ~ Party + era, data = f$corpus$meta,
            seed = 1, verbose = FALSE)
  # one topic-word distribution per *observed* Party x era combination
  expect_equal(length(fc$beta$logbeta), length(fc$settings$covariates$yvarlevels))
  expect_true(length(fc$beta$logbeta) <= np * ne)
  expect_equal(fc$settings$covariates$contentvars, c("Party", "era"))
  # marginal recovery by one covariate
  ct <- content_topics(fc, by = "era", n = 4)
  expect_named(ct, levels(f$corpus$meta$era))
  expect_equal(dim(ct[["pre"]]), c(4L, 4L))

  ## ACCURACY: crossing ~Party+era must be byte-identical to fitting a single
  ## content covariate built as the manual interaction factor (same engine).
  m2 <- f$corpus$meta
  m2$gh <- droplevels(interaction(as.factor(m2$Party), as.factor(m2$era), sep = ":", drop = TRUE))
  c2 <- f$corpus; c2$meta <- m2
  fm <- stm(c2, K = 4, content = ~ gh, data = m2, seed = 1, verbose = FALSE)
  lev <- fc$settings$covariates$yvarlevels
  mp <- match(lev, fm$settings$covariates$yvarlevels)
  expect_setequal(lev, fm$settings$covariates$yvarlevels)
  maxdiff <- max(vapply(seq_along(lev), function(g)
    max(abs(exp(fc$beta$logbeta[[g]]) - exp(fm$beta$logbeta[[mp[g]]]))), numeric(1)))
  expect_lt(maxdiff, 1e-12)
  ## content_topics marginal == manual average over the matching crossed groups
  gt <- fc$settings$covariates$contenttable
  gs <- gt$group[gt$era == "pre"]
  manual <- Reduce(`+`, lapply(gs, function(g) exp(fc$beta$logbeta[[g]]))) / length(gs)
  expect_equal(content_topics(fc, by = "era", n = 4)[["pre"]][1, ],
               f$corpus$vocab[order(-manual[1, ])[1:4]])
})

test_that("input validation rejects bad K and counts", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(5L)
  expect_error(stm(f$corpus, K = 1, verbose = FALSE), "K must be")
  m <- methods::as(quanteda::dfm(quanteda::tokens(quanteda::data_corpus_inaugural)), "CsparseMatrix")
  mneg <- m; mneg@x[1] <- -1
  expect_error(as_corpus(mneg), "non-negative")
  mfr <- m; mfr@x[1] <- 1.5
  expect_error(as_corpus(mfr), "integer")
})

test_that("fit carries settings$dim$wcounts$x so stm::labelTopics()$lift works", {
  skip_if_not_built(); skip_if_not_installed("quanteda"); skip_if_not_installed("stm")
  f <- make_fit(6L)
  expect_equal(length(f$fit$settings$dim$wcounts$x), length(f$fit$vocab))
  lt <- stm::labelTopics(f$fit, n = 5)        # would error without the wcounts slot
  fa <- label_topics(f$fit, n = 5)
  expect_true(all(mapply(setequal, asplit(lt$lift, 1), asplit(fa$lift, 1))))
})

test_that("s() matches stm::s default df (spline coefficients agree)", {
  skip_if_not_installed("stm")
  x <- c(rep(1:6, 3))
  expect_equal(unclass(s(x)), unclass(stm::s(x)), check.attributes = FALSE)
})

test_that("content models carry stm-shaped SAGE kappa (topica >= 0.24.1)", {
  skip_if_not_built(); skip_if_not_installed("quanteda")
  f <- make_fit(4L)
  fc <- stm(f$corpus, K = 4, content = ~ Party, data = f$corpus$meta, seed = 1, verbose = FALSE)
  skip_if(is.null(fc$beta$kappa), "topica build predates SAGE kappa (#237 / v0.24.1)")
  G <- length(fc$settings$covariates$yvarlevels)
  expect_equal(length(fc$beta$kappa$params), 4L + G + 4L * G)   # K + G + K*G
  expect_equal(length(fc$beta$kappa$m), length(fc$vocab))
  expect_equal(fc$settings$dim$A, G)
  skip_if_not_installed("stm")
  sage <- stm::sageLabels(fc, n = 5)
  expect_s3_class(sage, "sageLabels")
  expect_equal(sage$K, 4)
  expect_no_error(stm::labelTopics(fc, n = 5))
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

test_that("estimateEffect weights/cluster align by position (non-sequential meta rownames)", {
  skip_if_not_built()
  data(poliblog, package = "faSTM")
  out <- list(documents = poliblog$documents, vocab = poliblog$vocab, meta = poliblog$meta)
  # poliblog meta keeps original (non-1:n) rownames; weights/cluster must still map.
  expect_false(identical(rownames(out$meta), as.character(seq_len(nrow(out$meta)))))
  fit <- stm(out$documents, out$vocab, K = 6, prevalence = ~ rating, data = out$meta,
             max.em.its = 8L, verbose = FALSE)
  w <- with(out$meta, ifelse(rating == "Liberal", 1.3, 0.8))
  eff_w  <- estimateEffect(1:6 ~ rating, fit, metadata = out$meta, weights = w, nsims = 10L)
  eff_cl <- estimateEffect(1:6 ~ rating, fit, metadata = out$meta, cluster = out$meta$blog, nsims = 10L)
  eff_u  <- estimateEffect(1:6 ~ rating, fit, metadata = out$meta, nsims = 10L)
  expect_s3_class(eff_w, "faSTM_effect")
  expect_s3_class(eff_cl, "faSTM_effect")
  # weights actually change the estimate vs unweighted
  expect_false(isTRUE(all.equal(eff_w$coefficients[[1]]$est, eff_u$coefficients[[1]]$est)))
})
