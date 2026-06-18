#' Align a new corpus to a fitted model's vocabulary
#'
#' Maps a new corpus's terms onto the term indices of a fitted faSTM model,
#' dropping out-of-vocabulary terms — the preprocessing needed before inferring
#' topics for new documents (cf. `stm::alignCorpus`).
#'
#' @param newdata A `faSTM_corpus`, quanteda `dfm`, or document-term matrix.
#' @param model A faSTM fit.
#' @return A list with per-document `ids` (0-based indices into `model$vocab`)
#'   and `counts`, plus `dropped` (count of out-of-vocabulary term tokens).
#' @export
align_corpus <- function(newdata, model) {
  corpus <- as_corpus(newdata)
  map <- match(corpus$vocab, model$vocab)      # new term -> model term index (NA = OOV)
  dropped <- 0L
  per_doc <- lapply(corpus$documents, function(m) {
    mid <- map[m[1L, ]]; keep <- !is.na(mid)
    dropped <<- dropped + sum(as.integer(m[2L, !keep]))
    list(ids = as.integer(mid[keep]) - 1L, counts = as.numeric(m[2L, keep]))
  })
  list(per_doc = per_doc, dropped = dropped, n = length(per_doc))
}

#' Infer topic proportions for new documents
#'
#' Runs the variational E-step for each new document against the fitted model's
#' fixed global parameters (topic-word matrix, prior mean and covariance), giving
#' out-of-sample topic proportions (cf. `stm::fitNewDocuments`). The model's
#' topics are held fixed; only each new document's proportions are estimated.
#'
#' @param model A faSTM fit (non-content; for content models the group-marginal
#'   topic-word matrix is used, with a warning).
#' @param newdata A `faSTM_corpus`, quanteda `dfm`, or document-term matrix.
#'   Terms are aligned to the model's vocabulary; out-of-vocabulary terms are
#'   dropped.
#' @return A new-documents × K matrix of topic proportions.
#' @export
fit_new_documents <- function(model, newdata) {
  lb <- model$beta$logbeta
  if (length(lb) > 1L) {
    warning("content model: using the group-marginal topic-word matrix for ",
            "new-document inference.", call. = FALSE)
    beta <- Reduce(`+`, lapply(lb, exp)) / length(lb)
  } else {
    beta <- exp(lb[[1]])
  }
  K <- nrow(beta); V <- ncol(beta)
  mu <- model$mu$mu                            # K-1 (global prior mean)
  siginv <- model$invsigma                     # (K-1) x (K-1)

  al <- align_corpus(newdata, model)
  ids    <- unlist(lapply(al$per_doc, `[[`, "ids"),    use.names = FALSE)
  counts <- unlist(lapply(al$per_doc, `[[`, "counts"), use.names = FALSE)
  nterms <- vapply(al$per_doc, function(d) length(d$ids), integer(1L))
  if (al$dropped > 0L)
    message(sprintf("fit_new_documents: dropped %d out-of-vocabulary term token(s).",
                    al$dropped))

  theta_flat <- infer_theta_new(
    beta_flat  = as.double(t(beta)),           # K*V row-major
    num_topics = as.integer(K), num_types = as.integer(V),
    mu = as.double(mu), siginv = as.double(t(siginv)),
    words = as.integer(ids), counts = as.double(counts),
    doc_nterms = as.integer(nterms))

  matrix(theta_flat, nrow = al$n, ncol = K, byrow = TRUE)
}
