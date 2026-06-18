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
#' @param seed Optional seed for the posterior draws.
#' @return An object of class `c("faSTM_effect", "estimateEffect")` with a
#'   `summary()` method, holding pooled coefficients and standard errors per
#'   topic.
#' @export
estimateEffect <- function(formula, stmobj, metadata,
                           uncertainty = c("Global", "None"),
                           nsims = 100L, seed = NULL) {
  stopifnot(inherits(stmobj, "faSTM"))
  uncertainty <- match.arg(uncertainty)

  K <- ncol(stmobj$theta)
  topics <- .formula_topics(formula, K)
  rhs <- stats::reformulate(attr(stats::terms(formula), "term.labels"))
  X <- stats::model.matrix(rhs, data = metadata)

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

  out <- list(topics = topics, coefficients = per_topic,
              terms = colnames(X), formula = formula,
              uncertainty = uncertainty, nsims = length(draws))
  class(out) <- c("faSTM_effect", "estimateEffect")
  out
}

#' @export
summary.faSTM_effect <- function(object, ...) {
  tabs <- lapply(object$coefficients, function(c) {
    tval <- c$est / c$se
    data.frame(Estimate = c$est, `Std. Error` = c$se,
               `t value` = tval,
               `Pr(>|t|)` = 2 * stats::pt(-abs(tval), df = c$df),
               row.names = object$terms, check.names = FALSE)
  })
  names(tabs) <- names(object$coefficients)
  structure(list(tables = tabs, formula = object$formula,
                 uncertainty = object$uncertainty, nsims = object$nsims),
            class = "summary.faSTM_effect")
}

#' @export
print.summary.faSTM_effect <- function(x, ...) {
  cat("faSTM estimateEffect (", x$uncertainty, " uncertainty, ",
      x$nsims, " draws)\n", sep = "")
  for (nm in names(x$tables)) {
    cat("\n", nm, ":\n", sep = "")
    print(round(x$tables[[nm]], 4L))
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
  list(coef = fit$coefficients, vcov = s2 * xtxi, df = df)
}

# Rubin's rules: pool point estimates and within/between-draw variance.
.rubin_pool <- function(fits) {
  m <- length(fits)
  B <- vapply(fits, `[[`, numeric(length(fits[[1L]]$coef)), "coef")
  est <- rowMeans(B)
  within <- Reduce(`+`, lapply(fits, function(f) diag(f$vcov))) / m
  if (m > 1L) {
    between <- apply(B, 1L, stats::var)
    total <- within + (1 + 1 / m) * between
  } else {
    total <- within
  }
  list(est = est, se = sqrt(total), df = fits[[1L]]$df)
}

.formula_topics <- function(formula, K) {
  lhs <- formula[[2L]]
  if (length(formula) < 3L) return(seq_len(K))   # one-sided -> all topics
  topics <- eval(lhs, envir = list())
  if (is.null(topics) || !is.numeric(topics)) seq_len(K) else as.integer(topics)
}
