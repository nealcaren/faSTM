#' Fit a structural topic model (fast Rust backend, stm-compatible object)
#'
#' A drop-in replacement for [stm::stm()]'s fitting step. Accepts the same
#' `documents` / `vocab` / `prevalence` / `content` inputs, fits with topica's
#' Rust core, and returns an object compatible with the `stm` package so that
#' [stm::labelTopics()], [stm::plot.STM()], [stm::findThoughts()],
#' [stm::sageLabels()], and [stm::toLDAvis()] work unmodified. Use
#' [estimateEffect()] from this package for the honest covariate effects.
#'
#' @param documents stm-format documents: a named list of `2 x n_d` integer
#'   matrices (row 1 = 1-based word id into `vocab`, row 2 = count). Produced by
#'   [stm::prepDocuments()].
#' @param vocab Character vector of vocabulary terms.
#' @param K Number of topics.
#' @param prevalence A right-hand-side formula (e.g. `~ treatment + s(age)`) or a
#'   design matrix; topic prevalence covariates. `data` supplies the variables.
#' @param content A right-hand-side formula naming a single categorical variable,
#'   or a factor; the SAGE content covariate. `data` supplies the variable.
#' @param data A data.frame of document metadata (the `meta` from
#'   [stm::prepDocuments()]), aligned to `documents`.
#' @param max.em.its Maximum EM iterations (batch) / epochs (svi).
#' @param emtol Relative-bound convergence tolerance.
#' @param init.type Topic initialization: `"Spectral"` (stm's default),
#'   `"Random"`, `"LDA"` (seed from a quick CVB0 LDA, like stm's collapsed-Gibbs
#'   init), or `"Custom"` (seed from `init.beta` or a supplied `model`).
#' @param model A fitted model whose topic-word matrix seeds `init.type = "Custom"`.
#' @param init.beta Optional K x V topic-word probability matrix to start the fit
#'   from a given initialization (overrides `init.type`). Supplying R `stm`'s
#'   exact spectral beta here reproduces that run — a guaranteed
#'   "replicate the original" mode (topica #234/#235).
#' @param gamma.prior Prevalence-coefficient prior: `"Pooled"` (ridge, stm
#'   default) or `"L1"`.
#' @param sigma.prior Shrinkage applied to the topic covariance off-diagonal.
#' @param seed Integer seed (batch fit is reproducible from it).
#' @param inference `"batch"` (default, parity-validated) or `"svi"` (stochastic
#'   variational; scales to large corpora — requires a topica build with STM-SVI).
#' @param batch_size,tau,kappa SVI controls (minibatch size; Robbins-Monro
#'   `(tau + t)^(-kappa)` step schedule). Ignored when `inference = "batch"`.
#' @param num_threads Worker threads for the parallel variational E-step. `0`
#'   (default) uses all cores; `>= 1` pins a scoped pool. Results are identical
#'   regardless of thread count.
#' @param verbose Logical; print progress.
#'
#' @return An object of class `c("faSTM", "STM")` — an stm-compatible fit.
#' @export
stm <- function(documents, vocab, K,
                prevalence = NULL, content = NULL, data = NULL,
                max.em.its = 500L, emtol = 1e-5,
                init.type = c("Spectral", "Random", "LDA", "Custom"),
                init.beta = NULL, model = NULL,
                gamma.prior = c("Pooled", "L1"), gamma.l1.alpha = 1e-3,
                sigma.prior = 0,
                seed = 1L,
                inference = c("batch", "svi"),
                batch_size = 256L, tau = 64, kappa = 0.7,
                num_threads = 0L,
                verbose = TRUE, ...) {

  init.type <- match.arg(init.type)
  gamma.prior <- match.arg(gamma.prior)
  inference <- match.arg(inference)
  if (!is.numeric(K) || length(K) != 1L || K < 2L || K != as.integer(K))
    stop("K must be a single integer >= 2.", call. = FALSE)

  ## ---- stm-compatibility: accept stm-only controls; warn on genuine no-ops --
  dots <- list(...)
  if (isFALSE(dots$interactions))
    warning("content interactions=FALSE is not supported; faSTM's SAGE content ",
            "model always includes topic-by-covariate interactions.", call. = FALSE)
  if (!is.null(dots$kappa.prior) && !identical(dots$kappa.prior, "L1"))
    warning("kappa.prior = '", dots$kappa.prior, "' is not honored; faSTM uses ",
            "topica's content (SAGE) regularization.", call. = FALSE)
  ## LDAbeta/reportevery/control/etc. are accepted and ignored (no effect on the
  ## Rust fit); they exist so stm scripts run unmodified.

  ## init.type: 'Custom' seeds beta from `init.beta` (or a supplied `model`);
  ## 'LDA' seeds it from a quick CVB0 LDA below (after the token stream is built).
  if (init.type == "Custom" && is.null(init.beta)) {
    if (is.null(model) || is.null(model$beta$logbeta))
      stop("init.type = 'Custom' needs `init.beta` or a fitted `model`.", call. = FALSE)
    init.beta <- exp(model$beta$logbeta[[1L]])
  }

  ## ---- ingest: accept a faSTM corpus / quanteda dfm / matrix, or an
  ##      stm-style (documents list + vocab) input ---------------------------
  if (inherits(documents, "faSTM_corpus") || inherits(documents, "dfm") ||
      is.matrix(documents) || methods::is(documents, "Matrix")) {
    corpus <- as_corpus(documents, meta = data)
  } else if (is.list(documents)) {
    if (is.null(vocab)) stop("vocab is required when `documents` is a documents list.",
                             call. = FALSE)
    corpus <- .stm_documents_to_corpus(documents, vocab, data)
  } else {
    stop("`documents` must be a faSTM corpus, a quanteda dfm, a document-term ",
         "matrix, or an stm-style documents list (with `vocab`).", call. = FALSE)
  }
  documents <- corpus$documents; vocab <- corpus$vocab
  if (is.null(data)) data <- corpus$meta
  D <- length(documents); V <- length(vocab)
  dtm <- .documents_to_dtm(documents, V)

  ## ---- documents -> flat 0-based token stream ----------------------------
  ## stm doc = 2 x n integer matrix (1-based id; count). topica wants a token
  ## sequence with counts expanded. Concatenate; pass lengths alongside.
  expand_doc <- function(m) {
    ids <- as.integer(m[1L, ]) - 1L          # -> 0-based
    cts <- as.integer(m[2L, ])
    rep.int(ids, cts)
  }
  toks <- lapply(documents, expand_doc)
  doc_lens <- vapply(toks, length, integer(1L))
  docs_flat <- as.integer(unlist(toks, use.names = FALSE))

  ## ---- prevalence design -------------------------------------------------
  prev <- .make_design(prevalence, data, D)   # NULL or list(X = D x P, names)
  prevalence_flat <- if (is.null(prev)) NULL else as.double(t(prev$X)) # row-major
  num_features <- if (is.null(prev)) 0L else ncol(prev$X)

  ## ---- content (SAGE) group ids ------------------------------------------
  cont <- .make_content(content, data, D)     # NULL or list(group = 0-based, levels)
  content_groups <- if (is.null(cont)) NULL else as.integer(cont$group)
  num_groups <- if (is.null(cont)) 1L else length(cont$levels)
  ## Be explicit (not silent) when several content covariates are crossed — this
  ## is a deliberate extension beyond stm, which allows only one content variable.
  if (!is.null(cont) && length(cont$vars) > 1L && verbose)
    message(sprintf("faSTM: crossing %d content covariates (%s) into a saturated content model with %d groups.",
                    length(cont$vars), paste(cont$vars, collapse = ", "), num_groups))

  ## ---- svi gating --------------------------------------------------------
  if (inference == "svi" && (!is.null(prev) || !is.null(cont))) {
    stop("inference = \"svi\" with prevalence/content requires a topica build ",
         "that includes STM-SVI (topica #231 PR B). Use inference = \"batch\", ",
         "or pin a newer topica.", call. = FALSE)
  }

  ## ---- LDA initialization (stm's init.type = "LDA") ----------------------
  ## Seed beta from a quick CVB0 LDA (topica's deterministic collapsed VB), the
  ## faSTM analog of stm's collapsed-Gibbs LDA init; fed in as init.beta below.
  if (init.type == "LDA" && is.null(init.beta)) {
    lda.its <- if (is.null(dots$lda.its)) 50L else as.integer(dots$lda.its)
    lda_flat <- lda_init_beta(
      docs_flat = docs_flat, doc_lens = doc_lens,
      num_types = V, num_topics = as.integer(K),
      iters = lda.its, alpha = 50 / K, beta = 0.01, seed = as.integer(seed))
    B <- matrix(lda_flat, nrow = K, ncol = V, byrow = TRUE)  # K x V probs
    init.beta <- B / rowSums(B)                              # ensure rows sum to 1
  }

  ## ---- fit (single Rust call) --------------------------------------------
  if (verbose) message(sprintf("faSTM: fitting K=%d on %d docs (%s)...",
                               K, D, inference))
  raw <- fit_stm(
    docs_flat      = docs_flat,
    doc_lens       = doc_lens,
    num_types      = V,
    num_topics     = as.integer(K),
    em_iters       = as.integer(max.em.its),
    em_tol         = as.double(emtol),
    sigma_shrink   = as.double(sigma.prior),
    prevalence     = prevalence_flat,
    num_features   = num_features,
    content_groups = content_groups,
    num_groups     = num_groups,
    init_spectral  = identical(init.type, "Spectral"),
    init_beta      = if (is.null(init.beta)) NULL else as.double(t(init.beta)),  # K*V row-major
    gamma_l1_alpha = if (gamma.prior == "L1") as.double(gamma.l1.alpha) else NULL,
    diagonal       = FALSE,
    seed           = as.integer(seed),
    inference      = inference,
    batch_size     = as.integer(batch_size),
    tau            = as.double(tau),
    kappa          = as.double(kappa),
    num_threads    = as.integer(num_threads)
  )

  as_stm_object(raw, vocab = vocab,
                prevalence = prev, content = cont,
                word_counts = corpus$word_counts, dtm = dtm,
                documents = documents,
                call = match.call(), settings = list(
                  dim = list(K = as.integer(K), V = V, N = D,
                             A = num_groups),
                  init = list(mode = init.type),
                  inference = inference))
}

# stm-style documents list (+ vocab, meta) -> faSTM_corpus, via a dtm round-trip
# so empty-doc dropping / term re-indexing matches the corpus path.
.stm_documents_to_corpus <- function(documents, vocab, meta) {
  dtm <- .documents_to_dtm(documents, length(vocab))
  colnames(dtm) <- vocab
  .corpus_from_matrix(dtm, vocab, meta)
}

# per-document 2xn integer matrices -> D x V sparse document-term matrix
.documents_to_dtm <- function(documents, V) {
  ## vectorized (the old per-doc c() accumulation was O(n^2) — ~6s on 5k docs)
  ncols <- vapply(documents, ncol, integer(1L))
  i <- rep.int(seq_along(documents), ncols)
  j <- unlist(lapply(documents, function(m) m[1L, ]), use.names = FALSE)
  x <- unlist(lapply(documents, function(m) m[2L, ]), use.names = FALSE)
  Matrix::sparseMatrix(i = i, j = as.integer(j), x = as.numeric(x),
                       dims = c(length(documents), V))
}
