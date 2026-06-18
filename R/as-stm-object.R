#' Assemble a topica CtmModel result into an stm-compatible S3 object
#'
#' This is the compatibility seam: it reshapes the flat arrays returned by the
#' Rust `fit_stm()` into the slots stm's post-fit functions read. See
#' `DESIGN.md` for the field map. The returned object has class
#' `c("faSTM", "STM")` so it dispatches to stm's `labelTopics`, `plot`,
#' `findThoughts`, `sageLabels`, `toLDAvis`.
#'
#' @keywords internal
#' @noRd
as_stm_object <- function(raw, vocab, prevalence, content, call, settings,
                          word_counts = NULL, dtm = NULL, documents = NULL) {
  K <- raw$num_topics
  V <- raw$num_types
  D <- raw$num_docs
  Km1 <- K - 1L

  ## beta: K*V row-major prob -> list of log-beta matrices (one per content group)
  beta_prob <- matrix(raw$beta, nrow = K, ncol = V, byrow = TRUE)
  logbeta <- if (is.null(raw$content_beta)) {
    list(log(beta_prob))
  } else {
    G <- raw$num_groups
    cb <- raw$content_beta                     # G*K*V group-major flatten
    lapply(seq_len(G), function(g) {
      off <- (g - 1L) * K * V
      log(matrix(cb[(off + 1L):(off + K * V)], nrow = K, ncol = V, byrow = TRUE))
    })
  }

  ## lambda (D x (K-1)) -> stm `eta`; theta = softmax([eta, 0])
  eta <- matrix(raw$lambda, nrow = D, ncol = Km1, byrow = TRUE)
  theta <- .softmax_rows(cbind(eta, 0))        # D x K, reference topic appended

  ## per-doc Laplace covariance nu (D x (K-1)^2) -> list of (K-1)x(K-1) matrices
  nu <- lapply(seq_len(D), function(d) {
    matrix(raw$nu[((d - 1L) * Km1 * Km1 + 1L):(d * Km1 * Km1)],
           nrow = Km1, ncol = Km1, byrow = TRUE)
  })

  sigma <- matrix(raw$sigma, nrow = Km1, ncol = Km1, byrow = TRUE)

  gamma <- if (is.null(raw$gamma)) {
    matrix(0, nrow = 1L, ncol = Km1)
  } else {
    matrix(raw$gamma, ncol = Km1, byrow = TRUE) # num_features x (K-1)
  }

  ## stm reads corpus term frequencies from settings$dim$wcounts$x (used by the
  ## lift labels); populate it so stm::labelTopics()$lift works on faSTM fits.
  if (!is.null(word_counts)) settings$dim$wcounts <- list(x = as.numeric(word_counts))

  obj <- list(
    ## mu$mu as a (K-1) x 1 matrix (single global prior mean) — stm's
    ## thetaPosterior / simulation code indexes ncol(mu$mu).
    mu = list(mu = matrix(raw$mu, ncol = 1L), gamma = gamma),
    sigma = sigma,
    ## beta$logbeta is a per-group list for content models (what sage_labels()
    ## and the perspectives plot read). NOTE: faSTM does not reconstruct stm's
    ## additive SAGE kappa decomposition, so stm::sageLabels()/labelTopics() are
    ## not supported on content fits — use faSTM::sage_labels() instead.
    beta = list(logbeta = logbeta),
    settings = c(settings, list(
      covariates = list(X = if (is.null(prevalence)) NULL else prevalence$X,
                        betaindex = if (is.null(content)) rep(1L, D) else content$group + 1L,
                        yvarlevels = if (is.null(content)) NULL else content$levels),
      gamma = list(prior = "Pooled"),
      convergence = list(bound = raw$bound_history))),
    vocab = vocab,
    convergence = list(bound = raw$bound_history,
                       its = length(raw$bound_history),
                       converged = isTRUE(raw$converged)),
    theta = theta,
    eta = eta,
    invsigma = solve(sigma),
    nu = nu,                 # faSTM extension: per-doc posterior cov for effects
    ## faSTM extensions: corpus carried with the fit so inspection (FREX,
    ## coherence, exclusivity) is self-contained and needs no re-supply.
    word_counts = word_counts,
    dtm = dtm,
    documents = documents,
    call = call
  )
  class(obj) <- c("faSTM", "STM")
  obj
}

.softmax_rows <- function(m) {
  m <- m - apply(m, 1L, max)
  e <- exp(m)
  e / rowSums(e)
}

## ---- covariate helpers ----------------------------------------------------

#' @keywords internal
#' @noRd
.make_design <- function(prevalence, data, D) {
  if (is.null(prevalence)) return(NULL)
  if (inherits(prevalence, "formula")) {
    if (is.null(data)) stop("prevalence formula needs `data`.", call. = FALSE)
    X <- stats::model.matrix(prevalence, data = data)
  } else {
    X <- as.matrix(prevalence)
    if (!any(apply(X, 2L, function(z) all(z == 1)))) X <- cbind(`(Intercept)` = 1, X)
  }
  if (nrow(X) != D) stop("prevalence design has ", nrow(X), " rows, expected ", D,
                         call. = FALSE)
  list(X = X, names = colnames(X))
}

#' @keywords internal
#' @noRd
.make_content <- function(content, data, D) {
  if (is.null(content)) return(NULL)
  if (inherits(content, "formula")) {
    if (is.null(data)) stop("content formula needs `data`.", call. = FALSE)
    mf <- stats::model.frame(content, data = data)
    if (ncol(mf) != 1L)
      stop("content can only contain one variable (got ", ncol(mf), ").", call. = FALSE)
    v <- mf[[1L]]
  } else {
    v <- content
  }
  f <- as.factor(v)
  if (length(f) != D) stop("content variable has length ", length(f),
                           ", expected ", D, call. = FALSE)
  list(group = as.integer(f) - 1L, levels = levels(f)) # 0-based for Rust
}
