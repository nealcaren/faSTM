# stm-compatibility layer. Thin aliases (stm's camelCase names + argument
# conventions) so existing stm code — including the stm vignette — runs on faSTM
# with minimal edits. New code should prefer the snake_case API; these forward to
# it. stm-only base-graphics arguments (printlegend, xaxt, pch, ...) are accepted
# and ignored via `...`.

.docs_to_corpus <- function(documents, vocab, data = NULL) {
  if (inherits(documents, "faSTM_corpus")) documents
  else .stm_documents_to_corpus(documents, vocab, data)
}

#' @rdname stm-compat
#' @export
labelTopics <- function(model, topics = NULL, n = 7, frexweight = 0.5) {
  out <- label_topics(model, n = n, frexweight = frexweight)
  if (!is.null(topics)) {
    keep <- match(topics, out$topics)
    out$prob <- out$prob[keep, , drop = FALSE]; out$frex <- out$frex[keep, , drop = FALSE]
    out$lift <- out$lift[keep, , drop = FALSE]; out$score <- out$score[keep, , drop = FALSE]
    out$topics <- topics
  }
  out
}

#' @rdname stm-compat
#' @export
findThoughts <- function(model, texts = NULL, topics = NULL, n = 3, ...)
  find_thoughts(model, texts = texts, topics = topics, n = n)

#' @rdname stm-compat
#' @export
sageLabels <- function(model, n = 7) sage_labels(model, n = n)

#' @rdname stm-compat
#' @export
topicCorr <- function(model, method = "simple", cutoff = 0.01, ...) {
  tc <- topic_correlation(model, cutoff = cutoff)
  structure(c(tc, list(model = model)), class = "faSTM_topiccorr")
}

#' @rdname stm-compat
#' @export
fitNewDocuments <- function(model, documents, ...) fit_new_documents(model, documents)

#' @rdname stm-compat
#' @export
searchK <- function(documents, vocab, K, data = NULL, prevalence = NULL,
                    content = NULL, heldout = TRUE, cores = 1, ...) {
  search_k(.docs_to_corpus(documents, vocab, data), K = K, prevalence = prevalence,
           content = content, heldout = heldout, cores = cores, ...)
}

#' @rdname stm-compat
#' @export
selectModel <- function(documents, vocab, K, N = 10, data = NULL,
                        prevalence = NULL, content = NULL, runs = N, cores = 1, ...) {
  select_model(.docs_to_corpus(documents, vocab, data), K = K, N = N,
               prevalence = prevalence, content = content, cores = cores, ...)
}

#' @rdname stm-compat
#' @export
manyTopics <- function(documents, vocab, K, data = NULL, prevalence = NULL,
                       content = NULL, runs = 10, cores = 1, ...) {
  many_topics(.docs_to_corpus(documents, vocab, data), K = K, N = runs,
              prevalence = prevalence, content = content, cores = cores, ...)
}

#' @rdname stm-compat
#' @export
make.heldout <- function(documents, vocab, N = floor(0.1 * length(documents)),
                         proportion = 0.5, seed = NULL, ...) {
  ho <- make_heldout(.docs_to_corpus(documents, vocab), N = N,
                     proportion = proportion, seed = seed)
  ## expose stm-style $documents/$vocab alongside the faSTM corpus
  ho$documents <- ho$corpus$documents; ho$vocab <- ho$corpus$vocab
  ho
}

#' @rdname stm-compat
#' @export
eval.heldout <- function(model, heldout) eval_heldout(model, heldout)
