#' Build a faSTM corpus from prepared text
#'
#' faSTM does not do its own tokenization — it reads an already-prepared
#' document-term representation from the tools the field already uses
#' (`quanteda`, `tidytext`) or a plain sparse matrix. `as_corpus()` normalizes
#' any of these into the structure [stm()] consumes, dropping empty documents
#' and re-indexing the vocabulary, with metadata kept aligned.
#'
#' @param x A `quanteda` `dfm`, a document-term `Matrix`/matrix (documents in
#'   rows, terms in columns, with `colnames`), or an existing `faSTM_corpus`.
#'   For a tidy (long) term table use [from_tidy()].
#' @param meta Optional data.frame of document metadata, one row per document,
#'   aligned to `x`. For a `dfm`, defaults to `quanteda::docvars(x)`.
#' @param ... Unused.
#'
#' @return A `faSTM_corpus`: a list with `documents` (named list of 2×n integer
#'   matrices: row 1 = 1-based term id, row 2 = count), `vocab` (character),
#'   `meta` (data.frame or NULL), and `word_counts` (corpus term frequencies).
#' @export
as_corpus <- function(x, meta = NULL, ...) UseMethod("as_corpus")

#' @export
as_corpus.faSTM_corpus <- function(x, meta = NULL, ...) x

#' @export
as_corpus.dfm <- function(x, meta = NULL, ...) {
  if (is.null(meta) && requireNamespace("quanteda", quietly = TRUE)) {
    dv <- quanteda::docvars(x)
    if (ncol(dv) > 0) meta <- as.data.frame(dv)
  }
  vocab <- colnames(x)
  .corpus_from_matrix(methods::as(x, "CsparseMatrix"), vocab, meta)
}

#' @export
as_corpus.default <- function(x, meta = NULL, ...) {
  m <- methods::as(Matrix::Matrix(x, sparse = TRUE), "CsparseMatrix")
  vocab <- colnames(x)
  if (is.null(vocab)) stop("matrix input needs column names (the vocabulary).", call. = FALSE)
  .corpus_from_matrix(m, vocab, meta)
}

#' Build a faSTM corpus from a tidy (long) term-count table
#'
#' For `tidytext`-style data: one row per (document, term) with a count.
#'
#' @param data A data.frame.
#' @param document,term,count Column names (strings) for the document id, the
#'   term, and the count. `count` defaults to a count of rows per (doc, term).
#' @param meta Optional per-document metadata, aligned to the sorted unique
#'   documents.
#' @return A `faSTM_corpus`.
#' @export
from_tidy <- function(data, document = "document", term = "term",
                      count = "n", meta = NULL) {
  stopifnot(all(c(document, term) %in% names(data)))
  docs  <- factor(data[[document]])
  terms <- factor(data[[term]])
  cts   <- if (count %in% names(data)) as.numeric(data[[count]]) else rep(1, nrow(data))
  m <- Matrix::sparseMatrix(i = as.integer(docs), j = as.integer(terms), x = cts,
                            dims = c(nlevels(docs), nlevels(terms)),
                            dimnames = list(levels(docs), levels(terms)))
  .corpus_from_matrix(methods::as(m, "CsparseMatrix"), levels(terms), meta)
}

# Core: docs x V CsparseMatrix -> faSTM_corpus, dropping empty docs/terms and
# re-indexing, keeping metadata aligned to surviving documents.
.corpus_from_matrix <- function(m, vocab, meta) {
  if (length(vocab) != ncol(m)) stop("vocab length != number of columns.", call. = FALSE)
  if (!is.null(meta) && nrow(meta) != nrow(m))
    stop("meta has ", nrow(meta), " rows but the corpus has ", nrow(m), " documents.",
         call. = FALSE)

  ## validate counts: must be finite, non-negative integers (bad counts otherwise
  ## get silently rounded / drop documents)
  if (length(m@x)) {
    if (anyNA(m@x) || any(!is.finite(m@x)))
      stop("document-term counts must be finite (found NA/Inf).", call. = FALSE)
    if (any(m@x < 0))
      stop("document-term counts must be non-negative.", call. = FALSE)
    if (any(m@x != round(m@x)))
      stop("document-term counts must be integers (found fractional values).", call. = FALSE)
  }

  ## drop zero-count terms, re-index vocab
  tf <- Matrix::colSums(m)
  keep_term <- tf > 0
  if (!all(keep_term)) { m <- m[, keep_term, drop = FALSE]; vocab <- vocab[keep_term]; tf <- tf[keep_term] }

  ## drop empty documents, realign meta
  dl <- Matrix::rowSums(m)
  keep_doc <- dl > 0
  ndrop <- sum(!keep_doc)
  if (ndrop > 0) {
    m <- m[keep_doc, , drop = FALSE]
    if (!is.null(meta)) meta <- meta[keep_doc, , drop = FALSE]
    message(sprintf("faSTM: dropped %d empty document(s).", ndrop))
  }

  ## per-document 2xn integer matrices (term id; count)
  mt <- methods::as(m, "TsparseMatrix")
  ord <- order(mt@i, mt@j)
  ii <- mt@i[ord] + 1L; jj <- mt@j[ord] + 1L; xx <- as.integer(round(mt@x[ord]))
  D <- nrow(m)
  documents <- vector("list", D)
  starts <- which(!duplicated(ii)); ends <- c(starts[-1] - 1L, length(ii))
  rowdoc <- ii[starts]
  for (r in seq_along(rowdoc)) {
    idx <- starts[r]:ends[r]
    documents[[rowdoc[r]]] <- rbind(jj[idx], xx[idx])
  }
  nm <- rownames(m); names(documents) <- if (is.null(nm)) as.character(seq_len(D)) else nm

  structure(list(documents = documents, vocab = vocab,
                 meta = if (is.null(meta)) NULL else as.data.frame(meta),
                 word_counts = as.integer(tf)),
            class = "faSTM_corpus")
}

#' @export
print.faSTM_corpus <- function(x, ...) {
  cat(sprintf("<faSTM_corpus> %d documents, %d vocabulary terms%s\n",
              length(x$documents), length(x$vocab),
              if (is.null(x$meta)) "" else sprintf(", %d metadata columns", ncol(x$meta))))
  invisible(x)
}
