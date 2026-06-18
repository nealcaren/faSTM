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
topicCorr <- function(model, method = c("simple", "huge"), cutoff = 0.01, verbose = TRUE, ...) {
  method <- match.arg(method)
  cormat <- stats::cor(model$theta)
  out <- list()
  if (method == "simple") {
    adjmat <- ifelse(cormat > cutoff, 1, 0)         # matches stm (diagonal kept)
    out$posadj <- adjmat
    out$poscor <- cormat * adjmat
    out$cor    <- ifelse(abs(cormat) > cutoff, cormat, 0)
  } else {
    if (!requireNamespace("huge", quietly = TRUE))
      stop("topicCorr(method = \"huge\") needs the 'huge' package.", call. = FALSE)
    Xn  <- huge::huge.npn(model$theta, verbose = verbose)
    ric <- huge::huge.select(huge::huge(Xn, nlambda = 30, verbose = verbose), verbose = verbose)
    out$posadj <- ric$refit * (cormat > 0)
    out$poscor <- ric$refit * (cormat > 0) * cormat
    out$cor    <- ric$refit * cormat
  }
  ## faSTM plot method reads the model from an attribute, so names() == stm's.
  attr(out, "faSTM_model") <- model
  class(out) <- c("faSTM_topiccorr", "topicCorr")
  out
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

#' @rdname stm-compat
#' @param nsims Posterior draws.
#' @param type Accepted for stm compatibility ("Global"/"Local"); both draw from
#'   the per-document Laplace posterior.
#' @export
thetaPosterior <- function(model, nsims = 100, type = "Global", documents = NULL, ...)
  posterior_theta_samples(model, nsims = nsims)

#' @rdname stm-compat
#' @param documents Accepted for stm compatibility; faSTM reads its stored corpus.
#' @param M Top words for coherence/exclusivity.
#' @export
topicQuality <- function(model, documents = NULL, M = 10L, ...) {
  qual <- data.frame(topic = seq_len(ncol(model$theta)),
                     coherence = semantic_coherence(model, M = M),
                     exclusivity = exclusivity(model, M = M))
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(qual, ggplot2::aes(.data$coherence, .data$exclusivity)) +
      ggplot2::geom_text(ggplot2::aes(label = .data$topic)) +
      ggplot2::labs(x = "semantic coherence", y = "exclusivity", title = "Topic quality") +
      ggplot2::theme_minimal(base_size = 12)
    print(p)
  }
  invisible(qual)
}

#' @rdname stm-compat
#' @param ... Passed to [LDAvis::serVis()].
#' @export
toLDAvis <- function(model, documents = NULL, ...) {
  if (!requireNamespace("LDAvis", quietly = TRUE))
    stop("toLDAvis() needs the 'LDAvis' package: install.packages('LDAvis').", call. = FALSE)
  docs <- if (!is.null(documents)) documents else model$documents
  if (is.null(docs)) stop("need the fitted documents (refit on a faSTM corpus/dfm).", call. = FALSE)
  json <- LDAvis::createJSON(
    phi = exp(model$beta$logbeta[[1]]),                  # K x V topic-word
    theta = model$theta,                                 # D x K
    doc.length = vapply(docs, function(m) sum(m[2L, ]), numeric(1)),
    vocab = model$vocab,
    term.frequency = model$word_counts)
  LDAvis::serVis(json, ...)
}
