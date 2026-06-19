#' Draw from the per-document topic-proportion posterior
#'
#' The variational (Laplace) posterior of each document's logit-topic vector is
#' `eta_d ~ N(lambda_d, nu_d)`, both stored on a faSTM fit. This draws `nsims`
#' samples of theta per document by sampling eta and applying the softmax (with
#' the reference topic appended as 0). This is the pure-R equivalent of topica's
#' `posterior_theta_samples`; no Rust call is needed because `eta` + `nu` fully
#' describe the posterior. Feeds [estimateEffect()]'s method of composition.
#'
#' @param model A faSTM fit (from [stm()]).
#' @param nsims Number of posterior draws.
#' @param seed Optional integer seed for reproducible draws.
#' @return A `nsims`-length list of `D x K` theta matrices.
#' @export
posterior_theta_samples <- function(model, nsims = 100L, seed = NULL) {
  stopifnot(inherits(model, "faSTM"))
  if (!is.null(seed)) {
    old <- .Random.seed_state(); on.exit(.restore_seed(old), add = TRUE)
    set.seed(seed)
  }
  eta <- model$eta                 # D x (K-1)
  nu <- model$nu                   # list of (K-1)x(K-1)
  D <- nrow(eta); Km1 <- ncol(eta)
  ## Precompute each document's covariance factor ONCE (chol, eigen fallback for
  ## non-PD) and sample all draws per doc as eta_d + A_d %*% Z — avoids the
  ## per-(doc,draw) decomposition MASS::mvrnorm would redo (the old hot loop).
  fac <- lapply(nu, function(S) {
    R <- tryCatch(chol(S), error = function(e) NULL)
    if (!is.null(R)) t(R)
    else { e <- eigen(S, symmetric = TRUE); e$vectors %*% diag(sqrt(pmax(e$values, 0)), Km1) }
  })
  samp <- lapply(seq_len(D), function(d)
    eta[d, ] + fac[[d]] %*% matrix(stats::rnorm(Km1 * nsims), Km1, nsims))  # Km1 x nsims
  lapply(seq_len(nsims), function(s) {
    etas <- t(vapply(samp, function(m) m[, s], numeric(Km1)))               # D x Km1
    .softmax_rows(cbind(etas, 0))
  })
}

.Random.seed_state <- function() if (exists(".Random.seed", .GlobalEnv))
  get(".Random.seed", .GlobalEnv) else NULL
.restore_seed <- function(s) if (!is.null(s)) assign(".Random.seed", s, .GlobalEnv)
