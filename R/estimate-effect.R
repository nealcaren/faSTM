#' Estimate covariate effects on topic prevalence (method of composition)
#'
#' A drop-in for [stm::estimateEffect()] that propagates per-document
#' topic-estimation uncertainty honestly: it regresses each posterior draw of
#' topic proportions on the covariates and pools the per-draw fits by Rubin's
#' rules. This is topica's "honest" effect estimator, the reason faSTM ships its
#' own rather than inheriting stm's.
#'
#' @param formula A formula whose LHS lists topic numbers (e.g. `1:5 ~ treatment`)
#'   or whose LHS is empty to use all topics; RHS gives the covariates.
#' @param stmobj A faSTM fit (from [stm()]).
#' @param metadata A data.frame of covariates aligned to the documents.
#' @param uncertainty `"Global"` (method of composition over posterior draws,
#'   default) or `"None"` (single OLS on the posterior-mean theta).
#' @param nsims Posterior draws for `uncertainty = "Global"`.
#' @param combine Optional list of topic vectors to also estimate as aggregate
#'   topics (each set's proportions are summed before regressing); named entries
#'   set the coefficient names. E.g. `combine = list(econ = c(3, 7))`.
#' @param seed Optional seed for the posterior draws.
#' @return An object of class `c("faSTM_effect", "estimateEffect")` with a
#'   `summary()` method, holding pooled coefficients and standard errors per
#'   topic.
#' @export
estimateEffect <- function(formula, stmobj, metadata = meta,
                           uncertainty = c("Global", "None", "Local"),
                           nsims = 100L, seed = NULL, meta = NULL,
                           documents = NULL, combine = NULL, ...) {
  stopifnot(inherits(stmobj, "faSTM"))
  if (is.null(metadata)) stop("supply document metadata via `metadata=` (or `meta=`).", call. = FALSE)
  uncertainty <- match.arg(uncertainty)
  ## faSTM's method of composition draws from each document's own Laplace
  ## covariance (nu) — i.e. stm's "Local", the accurate option. "Global" is
  ## accepted and uses the same per-document draws (faSTM does not fall back to
  ## stm's cheaper shared-covariance approximation). `documents` is accepted for
  ## stm signature compatibility; faSTM reads nu from the fit.

  K <- ncol(stmobj$theta)
  topics <- .formula_topics(formula, K)
  rhs <- stats::reformulate(attr(stats::terms(formula), "term.labels"))
  mf <- stats::model.frame(rhs, data = metadata)
  mterms <- stats::terms(mf)                      # carries spline knots (predvars)
  xlevels <- stats::.getXlevels(mterms, mf)       # factor levels, for safe prediction
  X <- stats::model.matrix(mterms, mf)

  if (uncertainty == "None") {
    draws <- list(stmobj$theta)
  } else {
    draws <- posterior_theta_samples(stmobj, nsims = nsims, seed = seed)
  }

  per_topic <- lapply(topics, function(k) {
    fits <- lapply(draws, function(th) .ols(X, th[, k]))
    .rubin_pool(fits)
  })
  names(per_topic) <- paste0("topic", topics)

  ## combine topics: estimate the effect on a summed set of topics' proportions,
  ## treating them as one aggregate topic (stm issue #200).
  if (!is.null(combine)) {
    if (!is.list(combine)) combine <- list(combine)
    if (is.null(names(combine)))
      names(combine) <- vapply(combine, function(s) paste0("topics", paste(s, collapse = "+")),
                               character(1))
    comb <- lapply(combine, function(set) {
      fits <- lapply(draws, function(th) .ols(X, rowSums(th[, set, drop = FALSE])))
      .rubin_pool(fits)
    })
    per_topic <- c(per_topic, comb)
  }

  out <- list(topics = topics, coefficients = per_topic,
              terms = colnames(X), formula = formula, metadata = metadata,
              mterms = mterms, xlevels = xlevels,
              uncertainty = uncertainty, nsims = length(draws))
  class(out) <- c("faSTM_effect", "estimateEffect")
  out
}

#' @export
summary.faSTM_effect <- function(object, topics = NULL,
                                 p.adjust.method = "none", ...) {
  coefs <- object$coefficients
  if (!is.null(topics)) coefs <- coefs[paste0("topic", topics)]
  tabs <- lapply(coefs, function(c) {
    tval <- c$est / c$se
    pval <- 2 * stats::pt(-abs(tval), df = c$df)
    pval <- stats::p.adjust(pval, method = p.adjust.method)   # #224
    data.frame(Estimate = c$est, `Std. Error` = c$se,
               `t value` = tval, `Pr(>|t|)` = pval,
               row.names = object$terms, check.names = FALSE)
  })
  names(tabs) <- names(coefs)
  ## per-topic regression diagnostics (#255: df, F, R^2)
  diagnostics <- data.frame(
    topic      = names(coefs),
    r.squared  = vapply(coefs, function(c) c$r.squared  %||% NA_real_, numeric(1)),
    fstatistic = vapply(coefs, function(c) c$fstatistic %||% NA_real_, numeric(1)),
    df.num     = vapply(coefs, function(c) c$df.num %||% NA_integer_, numeric(1)),
    df.den     = vapply(coefs, function(c) c$df.den %||% c$df, numeric(1)),
    row.names = NULL, check.names = FALSE)
  structure(list(tables = tabs, diagnostics = diagnostics, formula = object$formula,
                 uncertainty = object$uncertainty, nsims = object$nsims,
                 p.adjust.method = p.adjust.method),
            class = "summary.faSTM_effect")
}

#' @export
print.summary.faSTM_effect <- function(x, ...) {
  cat("faSTM estimateEffect (", x$uncertainty, " uncertainty, ",
      x$nsims, " draws)\n", sep = "")
  if (!identical(x$p.adjust.method, "none"))
    cat("p-values adjusted: ", x$p.adjust.method, "\n", sep = "")
  for (nm in names(x$tables)) {
    cat("\n", nm, ":\n", sep = "")
    print(round(x$tables[[nm]], 4L))
    d <- x$diagnostics[x$diagnostics$topic == nm, ]
    if (nrow(d) && is.finite(d$r.squared))
      cat(sprintf("  R-squared: %.3f | F(%d,%d): %.2f\n",
                  d$r.squared, d$df.num, d$df.den, d$fstatistic))
  }
  invisible(x)
}

## ---- internals ------------------------------------------------------------

.ols <- function(X, y) {
  fit <- stats::lm.fit(X, y)
  n <- nrow(X); p <- ncol(X)
  rss <- sum(fit$residuals^2)
  df <- n - p
  s2 <- rss / df
  xtxi <- chol2inv(qr.R(qr(X)))
  tss <- sum((y - mean(y))^2)                       # regression diagnostics (#255)
  r2  <- if (tss > 0) 1 - rss / tss else NA_real_
  fst <- if (p > 1L && df > 0L && rss > 0) ((tss - rss) / (p - 1L)) / (rss / df) else NA_real_
  list(coef = fit$coefficients, vcov = s2 * xtxi, df = df,
       r.squared = r2, fstatistic = fst, df.num = p - 1L, df.den = df)
}

# Rubin's rules: pool point estimates and within/between-draw covariance. Returns
# the FULL pooled covariance so plots can form arbitrary contrasts/predictions.
.rubin_pool <- function(fits) {
  m <- length(fits)
  p <- length(fits[[1L]]$coef)
  B <- vapply(fits, `[[`, numeric(p), "coef")            # p x m
  est <- if (m > 1L) rowMeans(B) else B[, 1L]
  within <- Reduce(`+`, lapply(fits, `[[`, "vcov")) / m  # p x p
  if (m > 1L) {
    between <- stats::cov(t(B))                          # p x p
    total <- within + (1 + 1 / m) * between
  } else {
    total <- within
  }
  list(est = est, se = sqrt(diag(total)), vcov = total, df = fits[[1L]]$df,
       r.squared  = mean(vapply(fits, `[[`, numeric(1), "r.squared")),
       fstatistic = mean(vapply(fits, `[[`, numeric(1), "fstatistic")),
       df.num = fits[[1L]]$df.num, df.den = fits[[1L]]$df.den)
}

.formula_topics <- function(formula, K) {
  lhs <- formula[[2L]]
  if (length(formula) < 3L) return(seq_len(K))   # one-sided -> all topics
  topics <- eval(lhs, envir = list())
  if (is.null(topics) || !is.numeric(topics)) seq_len(K) else as.integer(topics)
}
