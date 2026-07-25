#' Group-contrast content trajectory over an ordered content covariate
#'
#' For a model fit with a `content_time` covariate (see [stm()]), trace how the
#' difference in a word's probability between two content groups moves across the
#' ordered periods. This is the quantity the smoothed content-time covariate makes
#' estimable: whether and how two groups' *wording* of a topic diverges over time.
#'
#' With `ci = TRUE` the function attaches a nonparametric document bootstrap
#' confidence interval: it resamples documents with replacement, refits the model
#' `B` times (using `fit_args` on `corpus`), and takes percentile intervals of the
#' contrast at each period. Because each bootstrap fit numbers its topics
#' differently, the target topic is realigned in every replicate by overlap with
#' `anchor_words`, so `anchor_words` is required when `ci = TRUE`.
#'
#' The bootstrap refits the full model `B` times and is therefore expensive; it is
#' opt-in, and `B` should be chosen accordingly. The intervals are wider where the
#' data are thin (typically the first and last periods, where a first-order random
#' walk is least constrained), which is the honest signal a point trajectory hides.
#'
#' @param object A fitted `STM`/`faSTM` object with a `content_time` covariate.
#' @param words Character vector of words to trace.
#' @param groups Length-2 character vector `c(g1, g2)` of base content-group levels;
#'   the contrast is `p(word | g1) - p(word | g2)`. Defaults to the first two base
#'   groups in the fit.
#' @param topic Integer topic index in `object`'s numbering (the point-estimate
#'   topic). Ignored when `anchor_words` is supplied.
#' @param anchor_words Character vector identifying the target topic by top-word
#'   overlap. Required when `ci = TRUE` (to realign topics across bootstrap refits);
#'   optional otherwise. If given, it overrides `topic`.
#' @param ci Logical; if `TRUE`, attach bootstrap confidence intervals.
#' @param corpus The `faSTM_corpus` the model was fit on (required when `ci = TRUE`).
#' @param fit_args Named list of arguments passed to [stm()] to refit each bootstrap
#'   replicate, e.g. `list(K = 12, prevalence = ~ party + s(congress),
#'   content = ~ party, content_time = ~ congress, content_prior = "l1")`. Required
#'   when `ci = TRUE`.
#' @param B Integer number of bootstrap replicates (default 50).
#' @param level Confidence level for the percentile interval (default 0.95).
#' @param seed Optional integer for reproducible resampling.
#'
#' @param cluster Optional name of a column in `corpus$meta` identifying resampling
#'   clusters (e.g. `"speaker"`). When set, the bootstrap resamples whole clusters
#'   with replacement rather than individual documents, so repeated observations from
#'   the same unit are not treated as independent. `NULL` (default) resamples
#'   documents.
#' @return A data frame with columns `word`, `period`, and `estimate`, and, when
#'   `ci = TRUE`, `conf.low` and `conf.high`.
#'
#' @examples
#' \donttest{
#' data(congress)
#' fit <- stm(congress, K = 12, prevalence = ~ party + s(congress),
#'            content = ~ party, content_time = ~ congress, content_prior = "l1")
#' econ <- c("tax", "budget", "deficit", "jobs", "spending", "economy")
#' # point estimate
#' content_trajectory(fit, words = c("tax", "wage"),
#'                    groups = c("Democrat", "Republican"), anchor_words = econ)
#' # with a (small, illustrative) bootstrap interval
#' content_trajectory(fit, words = c("tax", "wage"),
#'                    groups = c("Democrat", "Republican"), anchor_words = econ,
#'                    ci = TRUE, corpus = congress, B = 20, seed = 1,
#'                    fit_args = list(K = 12, prevalence = ~ party + s(congress),
#'                                    content = ~ party, content_time = ~ congress,
#'                                    content_prior = "l1"))
#' }
#' @export
content_trajectory <- function(object, words, groups = NULL, topic = NULL,
                               anchor_words = NULL, ci = FALSE, corpus = NULL,
                               fit_args = NULL, B = 50L, level = 0.95, seed = NULL,
                               cluster = NULL) {
  est <- .content_traj_from_fit(object, words, groups, topic, anchor_words)
  if (!isTRUE(ci)) return(est)

  if (is.null(anchor_words))
    stop("`anchor_words` is required when ci = TRUE (to realign topics across ",
         "bootstrap refits).", call. = FALSE)
  if (is.null(corpus) || is.null(fit_args))
    stop("`corpus` and `fit_args` are required when ci = TRUE.", call. = FALSE)
  docs <- corpus$documents
  n <- length(docs)
  clust <- if (is.null(cluster)) NULL else corpus$meta[[cluster]]
  if (!is.null(seed)) set.seed(seed)

  reps <- vector("list", B)
  for (b in seq_len(B)) {
    idx <- .boot_index(n, clust)
    fit_b <- tryCatch(
      do.call(stm, c(list(documents = docs[idx], vocab = corpus$vocab,
                          data = corpus$meta[idx, , drop = FALSE], verbose = FALSE),
                     fit_args)),
      error = function(e) NULL)
    if (is.null(fit_b)) next
    reps[[b]] <- .content_traj_from_fit(fit_b, words, groups, topic = NULL,
                                        anchor_words = anchor_words)$estimate
  }
  mat <- .boot_matrix(reps, B)
  a <- (1 - level) / 2
  est$conf.low  <- apply(mat, 1, stats::quantile, probs = a,     na.rm = TRUE)
  est$conf.high <- apply(mat, 1, stats::quantile, probs = 1 - a, na.rm = TRUE)
  attr(est, "B") <- ncol(mat)
  est
}

#' Partisan (group) content-divergence trajectory over an ordered content covariate
#'
#' For a model fit with a `content_time` covariate, measure how far apart two content
#' groups' *whole* word distributions for a topic are in each ordered period. Where
#' [content_trajectory()] traces one word at a time, this pools the entire vocabulary
#' into a single distributional distance per period, a more stable and interpretable
#' summary of whether two groups' wording of a topic is diverging over time. The
#' aggregate signal has a confidence interval that is tight relative to its scale,
#' where single-word contrasts do not.
#'
#' @inheritParams content_trajectory
#' @param measure Distance between the two groups' per-period word distributions:
#'   `"hellinger"` (default) or `"tv"` (total variation).
#'
#' @return A data frame with columns `period` and `divergence`, and, when
#'   `ci = TRUE`, `conf.low` and `conf.high`.
#'
#' @examples
#' \donttest{
#' data(congress)
#' fit <- stm(congress, K = 12, prevalence = ~ party + s(congress),
#'            content = ~ party, content_time = ~ congress, content_prior = "l1")
#' econ <- c("tax", "budget", "deficit", "jobs", "spending", "economy")
#' content_divergence(fit, groups = c("Democrat", "Republican"), anchor_words = econ)
#' }
#' @export
content_divergence <- function(object, groups = NULL, topic = NULL,
                               anchor_words = NULL, measure = c("hellinger", "tv"),
                               ci = FALSE, corpus = NULL, fit_args = NULL,
                               B = 50L, level = 0.95, seed = NULL, cluster = NULL) {
  measure <- match.arg(measure)
  est <- .content_div_from_fit(object, groups, topic, anchor_words, measure)
  if (!isTRUE(ci)) return(est)

  if (is.null(anchor_words))
    stop("`anchor_words` is required when ci = TRUE.", call. = FALSE)
  if (is.null(corpus) || is.null(fit_args))
    stop("`corpus` and `fit_args` are required when ci = TRUE.", call. = FALSE)
  docs <- corpus$documents; n <- length(docs)
  clust <- if (is.null(cluster)) NULL else corpus$meta[[cluster]]
  if (!is.null(seed)) set.seed(seed)

  reps <- vector("list", B)
  for (b in seq_len(B)) {
    idx <- .boot_index(n, clust)
    fit_b <- tryCatch(
      do.call(stm, c(list(documents = docs[idx], vocab = corpus$vocab,
                          data = corpus$meta[idx, , drop = FALSE], verbose = FALSE),
                     fit_args)),
      error = function(e) NULL)
    if (is.null(fit_b)) next
    reps[[b]] <- .content_div_from_fit(fit_b, groups, topic = NULL,
                                       anchor_words = anchor_words, measure)$divergence
  }
  mat <- .boot_matrix(reps, B)
  a <- (1 - level) / 2
  est$conf.low  <- apply(mat, 1, stats::quantile, probs = a,     na.rm = TRUE)
  est$conf.high <- apply(mat, 1, stats::quantile, probs = 1 - a, na.rm = TRUE)
  attr(est, "B") <- ncol(mat)
  est
}

# Assemble the successful bootstrap replicates into a matrix. A few failed refits
# (a degenerate resample dropping a period) are expected and dropped; every refit
# failing is not, and used to fall through to `cbind()` of nothing -> a cryptic
# `apply(NULL, ...)` error. Surface it clearly instead, and warn on a high failure
# rate so a NaN-ish band is not mistaken for genuine uncertainty.
.boot_matrix <- function(reps, B) {
  ok <- reps[!vapply(reps, is.null, logical(1))]
  if (length(ok) == 0L)
    stop("bootstrap produced no usable replicates: all ", B, " refits failed. ",
         "Check that fit_args reproduces the original fit (e.g. it must include ",
         "content_time=).", call. = FALSE)
  if (length(ok) < B %/% 2L)
    warning(B - length(ok), " of ", B, " bootstrap refits failed; the CI uses only ",
            length(ok), " replicates and may be unreliable.", call. = FALSE)
  do.call(cbind, ok)
}

# One bootstrap resample of document indices. With `clust` (a per-document cluster
# vector) it is a cluster bootstrap: whole clusters are drawn with replacement.
.boot_index <- function(n, clust) {
  if (is.null(clust)) return(sample.int(n, n, replace = TRUE))
  groups <- split(seq_len(n), clust)
  unlist(groups[sample.int(length(groups), length(groups), replace = TRUE)],
         use.names = FALSE)
}

# Per-period distributional distance between two groups for a single fit.
.content_div_from_fit <- function(fit, groups, topic, anchor_words, measure) {
  lb <- fit$beta$logbeta
  lev <- fit$settings$covariates$yvarlevels
  if (is.null(lev) || !any(grepl("@", lev)))
    stop("`object` was not fit with a content_time covariate.", call. = FALSE)
  parts <- do.call(rbind, strsplit(lev, "@", fixed = TRUE))
  base <- parts[, 1]; per <- parts[, 2]
  if (is.null(groups)) groups <- unique(base)[1:2]
  up <- unique(per); ord <- suppressWarnings(as.numeric(up))
  periods <- if (!any(is.na(ord))) up[order(ord)] else sort(up)
  vocab <- fit$vocab

  if (!is.null(anchor_words)) {
    avg <- Reduce(`+`, lb) / length(lb)
    k <- which.max(apply(avg, 1, function(r)
      sum(vocab[order(r, decreasing = TRUE)[1:20]] %in% anchor_words)))
  } else if (!is.null(topic)) {
    k <- as.integer(topic)
  } else stop("supply either `topic` or `anchor_words`.", call. = FALSE)

  dist <- function(pD, pR) {
    pD <- pD / sum(pD); pR <- pR / sum(pR)
    if (measure == "hellinger") sqrt(0.5 * sum((sqrt(pD) - sqrt(pR))^2))
    else 0.5 * sum(abs(pD - pR))
  }
  cell <- function(g, p) match(paste0(g, "@", p), lev)
  div <- vapply(periods, function(p) {
    i1 <- cell(groups[1], p); i2 <- cell(groups[2], p)
    if (is.na(i1) || is.na(i2)) return(NA_real_)
    dist(exp(lb[[i1]][k, ]), exp(lb[[i2]][k, ]))
  }, numeric(1))
  data.frame(period = periods, divergence = div, stringsAsFactors = FALSE)
}

# Extract the group-contrast trajectory from a single fitted model.
.content_traj_from_fit <- function(fit, words, groups, topic, anchor_words) {
  lb <- fit$beta$logbeta
  lev <- fit$settings$covariates$yvarlevels
  if (is.null(lev) || !any(grepl("@", lev)))
    stop("`object` was not fit with a content_time covariate ",
         "(no 'group@period' content levels found).", call. = FALSE)
  vocab <- fit$vocab
  parts <- do.call(rbind, strsplit(lev, "@", fixed = TRUE))
  base <- parts[, 1]; per <- parts[, 2]
  if (is.null(groups)) groups <- unique(base)[1:2]
  # order periods numerically when possible
  up <- unique(per)
  ord <- suppressWarnings(as.numeric(up))
  periods <- if (!any(is.na(ord))) up[order(ord)] else sort(up)

  # identify the target topic
  if (!is.null(anchor_words)) {
    avg <- Reduce(`+`, lb) / length(lb)
    k <- which.max(apply(avg, 1, function(r)
      sum(vocab[order(r, decreasing = TRUE)[1:20]] %in% anchor_words)))
  } else if (!is.null(topic)) {
    k <- as.integer(topic)
  } else {
    stop("supply either `topic` or `anchor_words`.", call. = FALSE)
  }

  cell <- function(g, p) match(paste0(g, "@", p), lev)
  out <- do.call(rbind, lapply(intersect(words, vocab), function(w) {
    wi <- match(w, vocab)
    data.frame(word = w, period = periods, estimate = vapply(periods, function(p) {
      i1 <- cell(groups[1], p); i2 <- cell(groups[2], p)
      if (is.na(i1) || is.na(i2)) return(NA_real_)
      exp(lb[[i1]][k, wi]) - exp(lb[[i2]][k, wi])
    }, numeric(1)), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}
