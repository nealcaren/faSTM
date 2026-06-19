# broom-style tidiers + a predict method, so faSTM objects slot into tidyverse
# model workflows. The tidy/glance/augment generics come from the lightweight
# `generics` package (re-exported here so `library(faSTM); tidy(fit)` just works).

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment

#' Tidy a faSTM fit (topic-term or document-topic distributions)
#'
#' @param x A faSTM fit.
#' @param matrix `"beta"` (topic-term probabilities), `"gamma"` (document-topic
#'   proportions), or `"frex"` (topic-term FREX scores).
#' @param ... Unused.
#' @return A tidy data.frame.
#' @exportS3Method generics::tidy faSTM
tidy.faSTM <- function(x, matrix = c("beta", "gamma", "frex"), ...) {
  matrix <- match.arg(matrix)
  if (matrix == "gamma") {
    th <- x$theta; K <- ncol(th); D <- nrow(th)
    return(data.frame(document = rep(seq_len(D), times = K),
                      topic = rep(seq_len(K), each = D),
                      gamma = as.numeric(th), stringsAsFactors = FALSE))
  }
  mat <- if (matrix == "beta") exp(x$beta$logbeta[[1]]) else unname(frex_scores(x))
  K <- nrow(mat); V <- ncol(mat)
  df <- data.frame(topic = rep(seq_len(K), each = V),
                   term = rep(x$vocab, times = K),
                   value = as.numeric(t(mat)), stringsAsFactors = FALSE)
  names(df)[3] <- matrix
  df
}

#' Tidy an estimateEffect fit (one row per term per topic)
#'
#' @param x A `faSTM_effect`.
#' @param ... Unused.
#' @return A data.frame: `topic`, `term`, `estimate`, `std.error`, `statistic`, `p.value`.
#' @exportS3Method generics::tidy faSTM_effect
tidy.faSTM_effect <- function(x, ...) {
  do.call(rbind, lapply(names(x$coefficients), function(nm) {
    co <- x$coefficients[[nm]]; t <- co$est / co$se
    data.frame(topic = nm, term = x$terms, estimate = co$est, std.error = co$se,
               statistic = t, p.value = 2 * stats::pt(-abs(t), df = co$df),
               row.names = NULL, stringsAsFactors = FALSE)
  }))
}

#' One-row model summary for a faSTM fit
#'
#' @param x A faSTM fit.
#' @param ... Unused.
#' @return A one-row data.frame.
#' @exportS3Method generics::glance faSTM
glance.faSTM <- function(x, ...) {
  data.frame(k = ncol(x$theta), docs = nrow(x$theta), terms = length(x$vocab),
             content_groups = length(x$beta$logbeta),
             iterations = x$convergence$its %||% NA_integer_,
             converged = isTRUE(x$convergence$converged),
             stringsAsFactors = FALSE)
}

#' Augment: most-likely topic for each document-term token
#'
#' Assigns each (document, term) cell to the topic maximizing
#' `theta[doc, k] * beta[k, term]` (cf. `tidytext::augment.STM`).
#'
#' @param x A faSTM fit (carries its DTM).
#' @param data Ignored (accepted for the generic).
#' @param ... Unused.
#' @return A data.frame: `document`, `term`, `count`, `.topic`.
#' @exportS3Method generics::augment faSTM
augment.faSTM <- function(x, data = NULL, ...) {
  dtm <- x$dtm
  if (is.null(dtm)) stop("augment() needs the fit's stored DTM.", call. = FALSE)
  beta <- exp(x$beta$logbeta[[1]]); theta <- x$theta
  td <- methods::as(dtm, "TsparseMatrix")
  d <- td@i + 1L; wv <- td@j + 1L
  scores <- theta[d, , drop = FALSE] * t(beta[, wv, drop = FALSE])   # ncell x K
  data.frame(document = d, term = x$vocab[wv], count = td@x,
             .topic = max.col(scores, ties.method = "first"),
             stringsAsFactors = FALSE)
}

#' Predict topic proportions for new documents
#'
#' @param object A faSTM fit.
#' @param newdata New documents (corpus / dfm / matrix / stm-style list).
#' @param ... Passed to [fit_new_documents()].
#' @return A new-documents x K matrix of topic proportions.
#' @exportS3Method stats::predict faSTM
predict.faSTM <- function(object, newdata, ...) {
  fit_new_documents(object, newdata)
}
