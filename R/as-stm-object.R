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

  ## SAGE κ decomposition (topica >= 0.24.1): build stm's beta$kappa structure so
  ## stm::sageLabels()/labelTopics() rank words by topic/covariate/interaction κ.
  ## params order: K topics, then G covariate levels, then K·G interactions
  ## (topic-major: row = (topic-1)*G + group, matching topica's layout).
  beta_kappa <- NULL
  if (!is.null(content) && !is.null(raw$kappa_m)) {
    G <- raw$num_groups
    kt <- matrix(raw$kappa_topic,       nrow = K, ncol = V, byrow = TRUE)
    kc <- matrix(raw$kappa_cov,         nrow = G, ncol = V, byrow = TRUE)
    ki <- matrix(raw$kappa_interaction, nrow = K * G, ncol = V, byrow = TRUE)
    params <- c(lapply(seq_len(K),     function(k) kt[k, ]),
                lapply(seq_len(G),     function(g) kc[g, ]),
                lapply(seq_len(K * G), function(i) ki[i, ]))
    beta_kappa <- list(m = raw$kappa_m, params = params)
    settings$kappa <- list(interactions = TRUE, fixedintercept = TRUE)
    settings$dim$A <- G
  }

  ## mu$mu is the prior mean per document: a (K-1) x D matrix mu_d = X_d gamma
  ## for prevalence models, else a single (K-1) x 1 global mean. stm's
  ## thetaPosterior recovers mean(nu) = Sigma - cov(eta - mu) and Choleskys it,
  ## which is only PD when mu is the per-document mean (else the between-document
  ## prevalence variance leaks in and breaks positive-definiteness). `prior`
  ## keeps the global mean for out-of-sample inference (fit_new_documents).
  mu_doc <- if (!is.null(prevalence)) t(prevalence$X %*% gamma) else matrix(raw$mu, ncol = 1L)

  obj <- list(
    mu = list(mu = mu_doc, gamma = gamma, prior = as.numeric(raw$mu)),
    sigma = sigma,
    ## beta$logbeta is a per-group list for content models; beta$kappa is the
    ## stm-shaped SAGE decomposition (topica >= 0.24.1), so stm::sageLabels() and
    ## stm::labelTopics() work on content fits.
    beta = c(list(logbeta = logbeta), if (!is.null(beta_kappa)) list(kappa = beta_kappa)),
    settings = c(settings, list(
      covariates = list(X = if (is.null(prevalence)) NULL else prevalence$X,
                        betaindex = if (is.null(content)) rep(1L, D) else content$group + 1L,
                        yvarlevels = if (is.null(content)) NULL else content$levels,
                        ## content covariate names + per-group level table (for
                        ## marginal recovery when content is crossed over >1 var)
                        contentvars = if (is.null(content)) NULL else content$vars,
                        contenttable = if (is.null(content)) NULL else content$group_table),
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
    ## SAGE content is categorical; numeric content covariates become one level
    ## per value, which is almost never intended (stm issue #245).
    num <- vapply(mf, function(z) is.numeric(z) && length(unique(z)) > 2L, logical(1))
    if (any(num))
      warning("content covariate(s) ", paste(names(mf)[num], collapse = ", "),
              " are numeric; SAGE treats them as categorical (one level per value). ",
              "Bin/factor them first if that isn't intended (cf. stm #245).", call. = FALSE)
    comps <- lapply(mf, as.factor)
    vars  <- names(mf)
  } else {
    comps <- list(as.factor(content)); vars <- "content"
  }
  ## One content covariate: as before. Several: the fully crossed (saturated)
  ## content model — each combination of levels gets its own SAGE topic-word
  ## distribution (topica fits a single content factor, so the cross is how
  ## faSTM honors multiple content covariates; strictly more flexible than an
  ## additive per-covariate kappa, which needs topica core support).
  f <- if (length(comps) == 1L) comps[[1L]]
       else droplevels(interaction(comps, sep = ":", drop = TRUE))
  if (length(f) != D) stop("content variable has length ", length(f),
                           ", expected ", D, call. = FALSE)
  ## per-crossed-group component levels (for marginal recovery by one covariate);
  ## built from the data so it is robust to ":" inside level names.
  gt <- data.frame(group = as.integer(f),
                   stats::setNames(lapply(comps, as.character), vars),
                   stringsAsFactors = FALSE, check.names = FALSE)
  gt <- unique(gt); gt <- gt[order(gt$group), , drop = FALSE]; rownames(gt) <- NULL
  list(group = as.integer(f) - 1L, levels = levels(f),   # 0-based for Rust
       vars = vars, group_table = gt)
}
