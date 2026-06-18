# stm-compatible helpers for prepared-corpus workflows. The calc*/checkBeta
# functions are faithful ports of stm's internals; the rest wrap faSTM equivalents.

.safelog <- function(x) log(pmax(x, .Machine$double.eps))

#' stm-compatible label scorers (FREX / lift / score)
#'
#' Ports of `stm:::calcfrex`/`calclift`/`calcscore`. Each takes a K x V `logbeta`
#' (log topic-word matrix) and returns a V x K matrix whose columns are the word
#' indices ordered most- to least-characteristic for each topic.
#'
#' @param logbeta K x V log topic-word matrix.
#' @param w FREX frequency/exclusivity weight.
#' @param wordcounts Corpus term frequencies (enables the James-Stein shrinkage).
#' @return A V x K matrix of ordered word indices.
#' @export
calcfrex <- function(logbeta, w = 0.5, wordcounts = NULL) {
  K <- nrow(logbeta); V <- ncol(logbeta)
  excl <- logbeta - matrix(.lse_cols(logbeta), K, V, byrow = TRUE)
  if (!is.null(wordcounts)) {
    ep <- exp(excl)
    ep <- vapply(seq_len(V), function(x) .js_estimate(ep[, x], wordcounts[x]), numeric(K))
    excl <- .safelog(ep)
  }
  freqscore <- apply(logbeta, 1L, rank) / V          # V x K
  exclscore <- apply(excl,    1L, rank) / V
  frex <- 1 / (w / freqscore + (1 - w) / exclscore)
  apply(frex, 2L, order, decreasing = TRUE)
}

#' @rdname calcfrex
#' @export
calclift <- function(logbeta, wordcounts) {
  emp.prob <- log(wordcounts) - log(sum(wordcounts))
  lift <- logbeta - rep(emp.prob, each = nrow(logbeta))
  apply(lift, 1L, order, decreasing = TRUE)
}

#' @rdname calcfrex
#' @export
calcscore <- function(logbeta) {
  ldascore <- exp(logbeta) * (logbeta - rep(colMeans(logbeta), each = nrow(logbeta)))
  apply(ldascore, 1L, order, decreasing = TRUE)
}

#' Flag words that load almost entirely on one topic
#'
#' Port of `stm:::checkBeta`: finds (topic, word) cells whose `exp(logbeta)`
#' exceeds `1 - tolerance` — words that are nearly exclusive to a single topic,
#' which can destabilize estimation.
#'
#' @param stmobject A faSTM/stm fit.
#' @param tolerance Threshold; a word with topic-probability `> 1 - tolerance` is flagged.
#' @return A list with `problemTopics`, `problemWords`, and error counts per content group.
#' @export
checkBeta <- function(stmobject, tolerance = 0.01) {
  if (tolerance < 1e-6) stop("Tolerance value too low.", call. = FALSE)
  betamatrix <- stmobject$beta$logbeta
  problemTopics <- problemWords <- topicErrorTotal <- wordErrorTotal <- list()
  for (i in seq_along(betamatrix)) {
    hit <- which(betamatrix[[i]] > log(1 - tolerance), arr.ind = TRUE)
    colnames(hit) <- c("Topic", "Word")
    problemWords[[i]]    <- hit[, c("Word", "Topic"), drop = FALSE]
    problemTopics[[i]]   <- unname(hit[, "Topic"])
    topicErrorTotal[[i]] <- length(problemTopics[[i]])
    wordErrorTotal[[i]]  <- nrow(hit)
  }
  list(problemTopics = problemTopics, topicErrorTotal = topicErrorTotal,
       problemWords = problemWords, wordErrorTotal = wordErrorTotal,
       check = all(vapply(wordErrorTotal, function(x) x == 0L, logical(1))))
}

#' Per-document variational E-step (stm-compatible)
#'
#' Port of `stm:::optimizeDocument`'s interface: infers one document's topic
#' proportions against fixed globals and returns its variational mean `lambda`
#' (eta), Laplace covariance `nu`, and `theta`.
#'
#' @param document A 2 x n integer matrix (1-based vocab ids; counts).
#' @param eta Ignored starting value (kept for signature compatibility).
#' @param mu Prior mean (length K-1).
#' @param beta K x V topic-word probability matrix.
#' @param sigma,sigmainv Prior covariance or its inverse (supply one).
#' @param ... Ignored (stm signature compatibility).
#' @return A list with `lambda`, `nu`, and `theta`.
#' @export
optimizeDocument <- function(document, eta, mu, beta, sigma = NULL,
                             sigmainv = NULL, ...) {
  if (is.null(sigmainv) && is.null(sigma))
    stop("supply sigma or sigmainv.", call. = FALSE)
  siginv <- if (!is.null(sigmainv)) sigmainv else solve(sigma)
  ids <- as.integer(document[1L, ]) - 1L
  counts <- as.numeric(document[2L, ])
  th <- .infer_one(beta, mu, siginv, ids, counts)        # length K
  K <- length(th); Km1 <- K - 1L
  pk <- pmax(th[seq_len(Km1)], 1e-12)
  lambda <- log(pk) - log(max(th[K], 1e-12))
  nu <- solve(siginv + sum(counts) * (diag(pk, Km1) - tcrossprod(pk)))
  list(lambda = lambda, nu = nu, theta = th)
}

#' Convert documents/vocab between corpus formats (stm-compatible)
#'
#' Port of `stm:::convertCorpus`. `"Matrix"` returns a documents x V sparse
#' dgCMatrix; `"lda"` returns the documents list (the lda/stm format).
#'
#' @param documents stm-style documents list.
#' @param vocab Vocabulary vector.
#' @param type `"Matrix"` or `"lda"`.
#' @return The corpus in the requested format.
#' @export
convertCorpus <- function(documents, vocab, type = c("Matrix", "lda", "slam")) {
  type <- match.arg(type)
  if (type == "lda") return(documents)
  m <- .documents_to_dtm(documents, length(vocab)); colnames(m) <- vocab
  if (type == "Matrix") return(m)
  if (!requireNamespace("slam", quietly = TRUE))
    stop("type = 'slam' needs the 'slam' package.", call. = FALSE)
  slam::as.simple_triplet_matrix(m)
}

#' Build a (sparse) design matrix for new data (stm-compatible)
#'
#' Port of `stm:::makeDesignMatrix`: builds the model matrix for `newData` using
#' the term structure and factor levels of `origData`.
#'
#' @param formula A model formula.
#' @param origData Data defining the terms/levels.
#' @param newData Data to build the matrix for.
#' @param sparse Return a sparse matrix.
#' @param ... Ignored.
#' @return A (sparse) design matrix.
#' @export
makeDesignMatrix <- function(formula, origData, newData, sparse = TRUE, ...) {
  termobj <- stats::delete.response(stats::terms(formula, data = origData))
  mf <- stats::model.frame(termobj, data = origData)
  mt <- attr(mf, "terms")
  xlev <- stats::.getXlevels(mt, mf)
  newmf <- stats::model.frame(mt, newData, xlev = xlev)
  if (sparse) Matrix::sparse.model.matrix(mt, newmf) else stats::model.matrix(mt, newmf)
}

#' Align a new corpus to a reference vocabulary (stm-compatible)
#'
#' stm-shaped counterpart to [align_corpus()]: reindexes `new`'s documents onto
#' `old.vocab`, dropping out-of-vocabulary terms (and empty documents).
#'
#' @param new An stm-style `list(documents, vocab)` or a `faSTM_corpus`.
#' @param old.vocab Reference vocabulary to align onto.
#' @param verbose Logical.
#' @return `list(documents, vocab, docs.removed, words.removed)`.
#' @export
alignCorpus <- function(new, old.vocab, verbose = TRUE) {
  if (inherits(new, "faSTM_corpus")) { docs <- new$documents; vocab <- new$vocab }
  else { docs <- new$documents; vocab <- new$vocab }
  map <- match(vocab, old.vocab)                  # new id -> old id (NA = OOV)
  newdocs <- lapply(docs, function(m) {
    oid <- map[m[1L, ]]; keep <- !is.na(oid)
    rbind(as.integer(oid[keep]), as.integer(m[2L, keep]))
  })
  keepdoc <- vapply(newdocs, function(m) ncol(m) > 0L, logical(1))
  if (verbose && any(!keepdoc))
    message(sprintf("alignCorpus: removed %d document(s) with no in-vocabulary terms.",
                    sum(!keepdoc)))
  list(documents = newdocs[keepdoc], vocab = old.vocab,
       docs.removed = which(!keepdoc), words.removed = vocab[is.na(map)])
}

#' Coerce inputs into an stm-style corpus (stm-compatible)
#'
#' Port of `stm::asSTMCorpus`'s role: accepts a `faSTM_corpus`, quanteda `dfm`, or
#' document-term matrix and returns `list(documents, vocab, data)` in stm format.
#'
#' @param documents A corpus/dfm/matrix, or an stm-style documents list.
#' @param vocab Vocabulary (when `documents` is already a documents list).
#' @param data Optional metadata.
#' @param ... Ignored.
#' @return `list(documents, vocab, data)`.
#' @export
asSTMCorpus <- function(documents, vocab = NULL, data = NULL, ...) {
  if (inherits(documents, "faSTM_corpus"))
    return(list(documents = documents$documents, vocab = documents$vocab,
                data = documents$meta))
  if (inherits(documents, "dfm") || is.matrix(documents) ||
      methods::is(documents, "Matrix")) {
    co <- as_corpus(documents, meta = data)
    return(list(documents = co$documents, vocab = co$vocab, data = co$meta))
  }
  list(documents = documents, vocab = vocab, data = data)
}

#' @rdname stm-compat
#' @export
readLdac <- function(filename, ...) read_ldac(filename)

#' @rdname stm-compat
#' @export
writeLdac <- function(documents, file, ...) write_ldac(documents, file)
