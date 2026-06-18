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
#' @param init.type Topic initialization; `"Spectral"` (stm's default) or
#'   `"Random"`.
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
                init.type = c("Spectral", "Random"),
                gamma.prior = c("Pooled", "L1"), gamma.l1.alpha = 1e-3,
                sigma.prior = 0,
                seed = 1L,
                inference = c("batch", "svi"),
                batch_size = 256L, tau = 64, kappa = 0.7,
                num_threads = 0L,
                verbose = TRUE) {

  init.type <- match.arg(init.type)
  gamma.prior <- match.arg(gamma.prior)
  inference <- match.arg(inference)

  ## ---- input checks ------------------------------------------------------
  stopifnot(is.list(documents), is.character(vocab), length(vocab) >= 1L)
  D <- length(documents)
  V <- length(vocab)

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

  ## ---- svi gating --------------------------------------------------------
  if (inference == "svi" && (!is.null(prev) || !is.null(cont))) {
    stop("inference = \"svi\" with prevalence/content requires a topica build ",
         "that includes STM-SVI (topica #231 PR B). Use inference = \"batch\", ",
         "or pin a newer topica.", call. = FALSE)
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
                call = match.call(), settings = list(
                  dim = list(K = as.integer(K), V = V, N = D,
                             A = num_groups),
                  init = list(mode = init.type),
                  inference = inference))
}
