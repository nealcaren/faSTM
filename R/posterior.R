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
  D <- nrow(eta)
  draws <- vector("list", nsims)
  for (s in seq_len(nsims)) {
    etas <- t(vapply(seq_len(D), function(d) {
      MASS::mvrnorm(1L, mu = eta[d, ], Sigma = nu[[d]])
    }, numeric(ncol(eta))))
    draws[[s]] <- .softmax_rows(cbind(etas, 0))
  }
  draws
}

.Random.seed_state <- function() if (exists(".Random.seed", .GlobalEnv))
  get(".Random.seed", .GlobalEnv) else NULL
.restore_seed <- function(s) if (!is.null(s)) assign(".Random.seed", s, .GlobalEnv)
