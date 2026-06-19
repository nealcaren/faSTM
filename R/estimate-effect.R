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
#' @param documents Accepted for stm compatibility (faSTM reads nu from the fit).
#' @param weights Optional per-document survey/sampling weights (weighted OLS).
#' @param cluster Optional per-document cluster ids for cluster-robust SEs.
#' @param ... Unused (stm signature compatibility).
#' @return An object of class `c("faSTM_effect", "estimateEffect")` with a
#'   `summary()` method, holding pooled coefficients and standard errors per
#'   topic.
#' @export
estimateEffect <- function(formula, stmobj, metadata = meta,
                           uncertainty = c("Global", "None", "Local"),
                           nsims = 100L, seed = NULL, meta = NULL,
                           documents = NULL, combine = NULL,
                           weights = NULL, cluster = NULL, ...) {
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
  ## survey/sampling weights (WLS) and cluster ids (cluster-robust SEs); aligned
  ## to the model rows that survived model.frame's NA handling.
  w <- if (is.null(weights)) NULL else as.numeric(weights)[as.integer(rownames(mf))]
  cl <- if (is.null(cluster)) NULL else cluster[as.integer(rownames(mf))]

  if (uncertainty == "None") {
    draws <- list(stmobj$theta)
  } else {
    draws <- posterior_theta_samples(stmobj, nsims = nsims, seed = seed)
  }

  per_topic <- lapply(topics, function(k) {
    fits <- lapply(draws, function(th) .ols(X, th[, k], w = w, cluster = cl))
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
      fits <- lapply(draws, function(th) .ols(X, rowSums(th[, set, drop = FALSE]), w = w, cluster = cl))
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

.ols <- function(X, y, w = NULL, cluster = NULL) {
  n <- nrow(X); p <- ncol(X)
  fit <- if (is.null(w)) stats::lm.fit(X, y) else stats::lm.wfit(X, y, w)
  resid <- fit$residuals
  df <- n - p
  XtXi <- if (is.null(w)) chol2inv(qr.R(qr(X))) else chol2inv(chol(crossprod(X * sqrt(w))))
  if (is.null(cluster)) {
    rss <- if (is.null(w)) sum(resid^2) else sum(w * resid^2)
    vcov <- (rss / df) * XtXi
  } else {
    ## cluster-robust sandwich: bread (X'WX)^-1, meat = sum_g (sum_i w_i X_i e_i)(...)'
    sc <- X * (if (is.null(w)) resid else w * resid)          # n x p score rows
    G <- split(seq_len(n), cluster)
    meat <- Reduce(`+`, lapply(G, function(g) tcrossprod(colSums(sc[g, , drop = FALSE]))))
    ng <- length(G)
    adj <- (ng / (ng - 1)) * ((n - 1) / (n - p))              # Stata-style finite-sample correction
    vcov <- adj * (XtXi %*% meat %*% XtXi)
  }
  ybar <- if (is.null(w)) mean(y) else stats::weighted.mean(y, w)
  tss <- if (is.null(w)) sum((y - ybar)^2) else sum(w * (y - ybar)^2)   # diagnostics (#255)
  rss <- if (is.null(w)) sum(resid^2) else sum(w * resid^2)
  r2  <- if (tss > 0) 1 - rss / tss else NA_real_
  fst <- if (p > 1L && df > 0L && rss > 0) ((tss - rss) / (p - 1L)) / (rss / df) else NA_real_
  list(coef = fit$coefficients, vcov = vcov, df = df,
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

#' Average marginal effects from an estimateEffect fit
#'
#' The average expected change in a topic's proportion per unit of a covariate
#' (continuous: average derivative; factor: average level-vs-reference contrast),
#' averaged over the observed data. Cleaner than reading raw coefficients,
#' especially with splines/interactions (cf. the `margins` package; stm #271).
#'
#' @param object A `faSTM_effect` (from [estimateEffect()]).
#' @param covariate Covariate name.
#' @param topics Topics to report (default: all in the fit).
#' @param h Step for the numeric derivative (continuous covariates); defaults to
#'   `0.01 * sd`.
#' @param ci Confidence level.
#' @return A data.frame: `topic`, `term`, `ame`, `se`, `lower`, `upper`.
#' @export
ame <- function(object, covariate, topics = object$topics, h = NULL, ci = 0.95) {
  stopifnot(inherits(object, "faSTM_effect"))
  meta <- object$metadata
  if (!covariate %in% names(meta)) stop("`covariate` not in the effect metadata.", call. = FALSE)
  design_of <- function(nd) {
    mf <- stats::model.frame(object$mterms, nd, xlev = object$xlevels, na.action = stats::na.pass)
    stats::model.matrix(object$mterms, mf, xlev = object$xlevels)
  }
  is_factor <- is.factor(meta[[covariate]]) || is.character(meta[[covariate]])
  if (is_factor) {                                   # average level-vs-reference contrast
    lv <- levels(as.factor(meta[[covariate]])); ref <- lv[1L]
    contrasts <- lapply(lv[-1L], function(l) {
      nd1 <- meta; nd1[[covariate]] <- factor(l,   levels = lv)
      nd0 <- meta; nd0[[covariate]] <- factor(ref, levels = lv)
      colMeans(design_of(nd1) - design_of(nd0))
    })
    names(contrasts) <- paste0(covariate, lv[-1L])
  } else {                                           # average numeric derivative
    hh <- if (is.null(h)) 0.01 * stats::sd(meta[[covariate]]) else h
    nd1 <- meta; nd1[[covariate]] <- meta[[covariate]] + hh
    nd0 <- meta; nd0[[covariate]] <- meta[[covariate]] - hh
    contrasts <- list(colMeans((design_of(nd1) - design_of(nd0)) / (2 * hh)))
    names(contrasts) <- covariate
  }
  z <- stats::qt(1 - (1 - ci) / 2, df = object$coefficients[[1L]]$df)
  rows <- list()
  for (k in topics) {
    co <- object$coefficients[[paste0("topic", k)]]
    for (nm in names(contrasts)) {
      cv <- contrasts[[nm]]
      est <- sum(cv * co$est); se <- sqrt(as.numeric(t(cv) %*% co$vcov %*% cv))
      rows[[length(rows) + 1L]] <- data.frame(topic = k, term = nm, ame = est, se = se,
                                              lower = est - z * se, upper = est + z * se)
    }
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}
