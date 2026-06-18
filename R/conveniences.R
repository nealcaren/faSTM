# Small stm-parity conveniences: s() spline terms, residual dispersion check,
# theta-as-data.frame, and topic lookup by word.

#' Spline term for prevalence formulas
#'
#' A b-spline basis for smooth covariate effects, e.g. `prevalence = ~ s(day)`.
#' Matches `stm::s()` exactly — including the `df = min(10, nval - 1)` default —
#' so spline-term coefficients agree with `stm`. (You can also use
#' [splines::bs()]/[splines::ns()] directly.)
#'
#' @param x Numeric predictor.
#' @param df Basis dimension; defaults to `min(10, length(unique(x)) - 1)`.
#' @param ... Passed to [splines::bs()].
#' @return A spline basis matrix (with class `"s"`).
#' @export
s <- function(x, df, ...) {
  if (inherits(x, "Date")) x <- as.numeric(x)
  if (missing(df)) df <- min(10, length(unique(x)) - 1L)
  obj <- splines::bs(x, df, ...)
  attr(obj, "class") <- c("s", attr(obj, "class"))
  obj
}

#' Residual dispersion check (is K large enough?)
#'
#' Multinomial residual dispersion (Taddy 2012; port of `stm::checkResiduals`).
#' A dispersion well above 1 suggests too few topics.
#'
#' @param model A faSTM fit (carries its documents).
#' @param tol Threshold for counting estimable residual cells.
#' @return A list with `dispersion`, `pvalue`, and `df`.
#' @export
check_residuals <- function(model, tol = 0.01) {
  documents <- model$documents
  if (is.null(documents)) stop("this fit has no stored documents.", call. = FALSE)
  beta <- lapply(model$beta$logbeta, exp)
  theta <- model$theta
  index <- model$settings$covariates$betaindex
  if (is.null(index)) index <- rep(1L, length(documents))
  K <- model$settings$dim$K; phat <- model$settings$dim$V
  n <- length(documents)
  d <- n * (K - 1) + K * (phat - 1)

  doc_resid <- function(doc, th, bet) {
    q <- as.numeric(th %*% bet); m <- sum(doc[2L, ])
    Nhat <- sum(q * m > tol)
    x <- numeric(ncol(bet)); x[doc[1L, ]] <- doc[2L, ]
    out <- sum((x^2 - 2 * x * q * m) / (m * q * (1 - q))) + sum(m * q / (1 - q))
    list(out = out, Nhat = Nhat)
  }
  D <- 0; Nhat <- 0
  for (i in seq_len(n)) {
    r <- doc_resid(documents[[i]], theta[i, ], beta[[index[i]]])
    D <- D + r$out; Nhat <- Nhat + r$Nhat
  }
  df <- Nhat - phat - d
  list(dispersion = D / df,
       pvalue = suppressWarnings(stats::pchisq(D, df = df, lower.tail = FALSE)),
       df = df)
}

#' Document-topic proportions as a data frame
#'
#' @param model A faSTM fit.
#' @param meta Optional metadata to bind alongside (defaults to none).
#' @return A data.frame with `document` and `Topic1..TopicK` columns (+ `meta`).
#' @export
make_dt <- function(model, meta = NULL) {
  theta <- model$theta
  out <- data.frame(document = seq_len(nrow(theta)), theta)
  names(out)[-1L] <- paste0("Topic", seq_len(ncol(theta)))
  if (!is.null(meta)) out <- cbind(out, meta)
  out
}

#' Find topics whose top words include given words
#'
#' @param model A faSTM fit.
#' @param words Character vector of query words.
#' @param n Top words per topic to search.
#' @param type Ranking metric: `"prob"`, `"frex"`, `"lift"`, or `"score"`.
#' @return Integer vector of matching topics.
#' @export
find_topic <- function(model, words, n = 20L, type = c("prob", "frex", "lift", "score")) {
  type <- match.arg(type)
  lab <- label_topics(model, n = n)[[type]]      # K x n
  which(apply(lab, 1L, function(tw) any(words %in% tw)))
}
