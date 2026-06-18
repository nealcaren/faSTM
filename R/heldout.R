# Held-out likelihood by document completion (cf. stm::make.heldout /
# eval.heldout). Hold out a fraction of each chosen document's term tokens, fit
# on the remainder, then score the held-out tokens under the fitted model's
# per-document topic proportions. Held-out terms are recorded as STRINGS so the
# score is robust to vocabulary re-indexing during the fit.

#' Create a held-out version of a corpus for document-completion validation
#'
#' @param corpus A `faSTM_corpus`.
#' @param N Number of documents to hold tokens out of (default: 10% of docs).
#' @param proportion Fraction of each chosen document's term *types* to hold out.
#' @param seed Optional RNG seed.
#' @return A list with `corpus` (training corpus, held-out tokens removed) and
#'   `missing` (per-document held-out terms + counts), class `faSTM_heldout`.
#' @export
make_heldout <- function(corpus, N = floor(0.1 * length(corpus$documents)),
                         proportion = 0.5, seed = NULL) {
  stopifnot(inherits(corpus, "faSTM_corpus"))
  if (!is.null(seed)) { old <- .seed_state(); on.exit(.seed_restore(old), add = TRUE); set.seed(seed) }
  vocab <- corpus$vocab
  D <- length(corpus$documents)
  chosen <- sort(sample.int(D, min(N, D)))

  docs <- corpus$documents
  missing <- list()
  for (d in chosen) {
    m <- docs[[d]]; terms <- m[1L, ]; counts <- m[2L, ]
    nt <- length(terms)
    if (nt < 2L) next
    k <- floor(proportion * nt)
    if (k < 1L) next
    hold <- sample.int(nt, k)
    if (length(hold) >= nt) hold <- hold[-1L]   # keep >=1 training term
    missing[[length(missing) + 1L]] <- list(
      doc = d, terms = vocab[terms[hold]], counts = counts[hold])
    docs[[d]] <- m[, -hold, drop = FALSE]
  }

  ## rebuild a training corpus from the trimmed documents (vocab may re-index;
  ## held-out terms are kept as strings so eval still maps them)
  dtm <- .documents_to_dtm(docs, length(vocab)); colnames(dtm) <- vocab
  train <- .corpus_from_matrix(dtm, vocab, corpus$meta)
  structure(list(corpus = train, missing = missing, nheld = length(missing)),
            class = "faSTM_heldout")
}

#' Evaluate held-out log-likelihood of a fit on a held-out set
#'
#' @param model A faSTM fit (trained on `heldout$corpus`).
#' @param heldout A `faSTM_heldout` (or its `missing` list).
#' @return Mean per-document held-out log-likelihood per token.
#' @export
eval_heldout <- function(model, heldout) {
  missing <- if (inherits(heldout, "faSTM_heldout")) heldout$missing else heldout
  beta <- exp(model$beta$logbeta[[1]])          # K x V
  vocab <- model$vocab; theta <- model$theta
  scores <- numeric(0)
  for (h in missing) {
    if (h$doc > nrow(theta)) next               # doc dropped in training (rare)
    cols <- match(h$terms, vocab); keep <- !is.na(cols)
    if (!any(keep)) next
    pw <- as.numeric(theta[h$doc, ] %*% beta[, cols[keep], drop = FALSE])
    ntok <- sum(h$counts[keep])
    scores <- c(scores, sum(h$counts[keep] * log(pw)) / ntok)
  }
  mean(scores)
}

#' @export
print.faSTM_heldout <- function(x, ...) {
  cat(sprintf("<faSTM_heldout> tokens held out of %d documents\n", x$nheld)); invisible(x)
}

.seed_state <- function() if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
.seed_restore <- function(s) if (!is.null(s)) assign(".Random.seed", s, .GlobalEnv)
