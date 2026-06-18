#' Align a new corpus to a fitted model's vocabulary
#'
#' Maps a new corpus's terms onto the term indices of a fitted faSTM model,
#' dropping out-of-vocabulary terms — the preprocessing needed before inferring
#' topics for new documents (cf. `stm::alignCorpus`).
#'
#' @param newdata A `faSTM_corpus`, quanteda `dfm`, or document-term matrix.
#' @param model A faSTM fit.
#' @return A list with per-document `ids` (0-based indices into `model$vocab`)
#'   and `counts`, plus `dropped` (count of out-of-vocabulary term tokens).
#' @export
align_corpus <- function(newdata, model) {
  corpus <- as_corpus(newdata)
  map <- match(corpus$vocab, model$vocab)      # new term -> model term index (NA = OOV)
  dropped <- 0L
  per_doc <- lapply(corpus$documents, function(m) {
    mid <- map[m[1L, ]]; keep <- !is.na(mid)
    dropped <<- dropped + sum(as.integer(m[2L, !keep]))
    list(ids = as.integer(mid[keep]) - 1L, counts = as.numeric(m[2L, keep]))
  })
  list(per_doc = per_doc, dropped = dropped, n = length(per_doc))
}

#' Infer topic proportions for new documents
#'
#' Runs the variational E-step for each new document against the fitted model's
#' fixed global parameters (topic-word matrix, prior mean and covariance), giving
#' out-of-sample topic proportions (cf. `stm::fitNewDocuments`). The model's
#' topics are held fixed; only each new document's proportions are estimated.
#'
#' @param model A faSTM fit (non-content; for content models the group-marginal
#'   topic-word matrix is used, with a warning).
#' @param newdata A `faSTM_corpus`, quanteda `dfm`, or document-term matrix.
#'   Terms are aligned to the model's vocabulary; out-of-vocabulary terms are
#'   dropped.
#' @return A new-documents × K matrix of topic proportions.
#' @export
fit_new_documents <- function(model, newdata) {
  lb <- model$beta$logbeta
  if (length(lb) > 1L) {
    warning("content model: using the group-marginal topic-word matrix for ",
            "new-document inference.", call. = FALSE)
    beta <- Reduce(`+`, lapply(lb, exp)) / length(lb)
  } else {
    beta <- exp(lb[[1]])
  }
  K <- nrow(beta); V <- ncol(beta)
  mu <- model$mu$prior                         # K-1 (global prior mean; mu$mu is per-doc)
  siginv <- model$invsigma                     # (K-1) x (K-1)

  al <- align_corpus(newdata, model)
  ids    <- unlist(lapply(al$per_doc, `[[`, "ids"),    use.names = FALSE)
  counts <- unlist(lapply(al$per_doc, `[[`, "counts"), use.names = FALSE)
  nterms <- vapply(al$per_doc, function(d) length(d$ids), integer(1L))
  if (al$dropped > 0L)
    message(sprintf("fit_new_documents: dropped %d out-of-vocabulary term token(s).",
                    al$dropped))

  theta_flat <- infer_theta_new(
    beta_flat  = as.double(t(beta)),           # K*V row-major
    num_topics = as.integer(K), num_types = as.integer(V),
    mu = as.double(mu), siginv = as.double(t(siginv)),
    words = as.integer(ids), counts = as.double(counts),
    doc_nterms = as.integer(nterms))

  matrix(theta_flat, nrow = al$n, ncol = K, byrow = TRUE)
}

#' Infer topics for new documents (stm-compatible signature)
#'
#' Drop-in for [stm::fitNewDocuments()]. Holds the fitted topics fixed and runs
#' the variational E-step for each new document. Supports stm's prior modes and
#' posterior return.
#'
#' @param model A faSTM fit.
#' @param documents New documents: a `faSTM_corpus`/`dfm`/matrix (aligned to the
#'   model vocabulary), or an stm-style list of 2 x n integer matrices indexed
#'   into `model$vocab`.
#' @param newData,origData Covariate frames for the new and original documents
#'   (used by `prevalencePrior = "Covariate"` to set each document's prior mean).
#' @param prevalence Prevalence formula (same RHS as the fit) for the covariate prior.
#' @param betaIndex Integer per-document content-group index (content models).
#' @param prevalencePrior `"Average"` (global prior mean, default) or `"Covariate"`
#'   (per-document mean from `prevalence`/`newData`).
#' @param contentPrior `"Covariate"` (use the group's topic-word matrix via
#'   `betaIndex`, default) or `"Average"` (group-marginal).
#' @param returnPosterior If `TRUE`, return `list(theta, eta, nu)` (per-document
#'   variational mean and Laplace covariance); otherwise a documents x K theta matrix.
#' @param verbose Logical.
#' @param ... Ignored (stm signature compatibility).
#' @return A theta matrix, or a posterior list when `returnPosterior = TRUE`.
#' @export
fitNewDocuments <- function(model, documents, newData = NULL, origData = NULL,
                            prevalence = NULL, betaIndex = NULL,
                            prevalencePrior = c("Average", "Covariate", "None"),
                            contentPrior = c("Covariate", "Average"),
                            returnPosterior = FALSE, verbose = TRUE, ...) {
  stopifnot(inherits(model, "faSTM"))
  prevalencePrior <- match.arg(prevalencePrior)
  contentPrior <- match.arg(contentPrior)
  K <- ncol(model$theta); Km1 <- K - 1L
  siginv <- model$invsigma

  perdoc <- .newdoc_tokens(model, documents)        # list(ids 0-based, counts)
  n <- length(perdoc)

  ## per-document prior mean (K-1)
  mu_list <- if (prevalencePrior == "Covariate" && !is.null(prevalence) && !is.null(newData)) {
    gamma <- model$mu$gamma                          # num_features x (K-1)
    X <- .newdoc_design(prevalence, newData, origData)
    if (ncol(X) != nrow(gamma))
      stop("prevalence design has ", ncol(X), " columns but the fit used ",
           nrow(gamma), "; pass the same formula/levels.", call. = FALSE)
    lapply(seq_len(n), function(d) as.numeric(X[d, ] %*% gamma))
  } else {
    rep(list(model$mu$prior), n)                     # Average / None: global prior
  }

  ## per-document topic-word matrix (content models pick a group)
  lb <- model$beta$logbeta
  beta_list <- if (length(lb) > 1L) {
    if (contentPrior == "Average" || is.null(betaIndex)) {
      bm <- Reduce(`+`, lapply(lb, exp)) / length(lb)
      rep(list(bm), n)
    } else {
      bi <- rep_len(as.integer(betaIndex), n)
      lapply(bi, function(g) exp(lb[[g]]))
    }
  } else {
    rep(list(exp(lb[[1L]])), n)
  }

  thetas <- matrix(0, n, K)
  for (d in seq_len(n))
    thetas[d, ] <- .infer_one(beta_list[[d]], mu_list[[d]], siginv,
                              perdoc[[d]]$ids, perdoc[[d]]$counts)

  if (!returnPosterior) return(thetas)

  ## eta exact from theta (logit vs reference topic); nu = logistic-normal
  ## Laplace covariance (siginv + N * multinomial Fisher info)^{-1}.
  eta <- matrix(0, n, Km1); nu <- vector("list", n)
  for (d in seq_len(n)) {
    p <- thetas[d, ]; pk <- pmax(p[seq_len(Km1)], 1e-12)
    eta[d, ] <- log(pk) - log(max(p[K], 1e-12))
    N <- sum(perdoc[[d]]$counts)
    nu[[d]] <- solve(siginv + N * (diag(pk, Km1) - tcrossprod(pk)))
  }
  list(theta = thetas, eta = eta, nu = nu)
}

# new documents -> per-doc list(ids 0-based into model vocab, counts)
.newdoc_tokens <- function(model, documents) {
  if (inherits(documents, "faSTM_corpus") || inherits(documents, "dfm") ||
      is.matrix(documents) || methods::is(documents, "Matrix"))
    return(align_corpus(documents, model)$per_doc)
  ## stm-style list of 2 x n integer matrices (1-based ids into model$vocab)
  lapply(documents, function(m)
    list(ids = as.integer(m[1L, ]) - 1L, counts = as.numeric(m[2L, ])))
}

# single-document variational E-step against fixed globals -> length-K theta
.infer_one <- function(beta, mu, siginv, ids, counts) {
  infer_theta_new(
    beta_flat  = as.double(t(beta)),
    num_topics = as.integer(nrow(beta)), num_types = as.integer(ncol(beta)),
    mu = as.double(mu), siginv = as.double(t(siginv)),
    words = as.integer(ids), counts = as.double(counts),
    doc_nterms = as.integer(length(ids)))
}

# design matrix for new documents with factor levels pinned from the original data
.newdoc_design <- function(formula, newData, origData) {
  rhs <- stats::reformulate(attr(stats::terms(formula), "term.labels"))
  if (is.null(origData)) return(stats::model.matrix(rhs, newData))
  mf0 <- stats::model.frame(rhs, data = origData)
  mt  <- stats::terms(mf0)
  xl  <- stats::.getXlevels(mt, mf0)
  stats::model.matrix(mt, stats::model.frame(mt, data = newData, xlev = xl), xlev = xl)
}
