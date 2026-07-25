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
                                        anchor_words = anchor_words, warn = FALSE)
  }
  keys <- paste(est$word, est$period)
  mat  <- .boot_matrix(reps, keys, function(df) paste(df$word, df$period),
                       "estimate", B)
  ci <- .boot_ci(mat, level)
  est$conf.low <- ci$low; est$conf.high <- ci$high
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
                                       anchor_words = anchor_words, measure,
                                       warn = FALSE)
  }
  keys <- as.character(est$period)
  mat  <- .boot_matrix(reps, keys, function(df) as.character(df$period),
                       "divergence", B)
  ci <- .boot_ci(mat, level)
  est$conf.low <- ci$low; est$conf.high <- ci$high
  attr(est, "B") <- ncol(mat)
  est
}

# Assemble the successful bootstrap replicates into a matrix aligned to the point
# estimate's rows. Refits that *throw* are dropped (all-failed is surfaced as a
# clear error, not a cryptic `apply(NULL, ...)`; a high failure rate warns). A refit
# that *succeeds* but whose resample drops a period yields a shorter frame -- we must
# not `cbind` raw vectors (silent recycling misaligns word/period across replicates),
# so each replicate is realigned to `ref_keys` by label, NA-filling missing cells.
.boot_matrix <- function(reps, ref_keys, keyfun, valcol, B) {
  ok <- reps[!vapply(reps, is.null, logical(1))]
  if (length(ok) == 0L)
    stop("bootstrap produced no usable replicates: all ", B, " refits failed. ",
         "Check that fit_args reproduces the original fit (e.g. it must include ",
         "content_time=).", call. = FALSE)
  if (length(ok) < B %/% 2L)
    warning(B - length(ok), " of ", B, " bootstrap refits failed; the CI uses only ",
            length(ok), " replicates and may be unreliable.", call. = FALSE)
  mat <- vapply(ok, function(df) df[[valcol]][match(ref_keys, keyfun(df))],
                numeric(length(ref_keys)))
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = length(ref_keys))
  mat
}

# Percentile CI per row, tolerating rows that are all-NA across replicates (an
# unsampled (word, )period in every kept refit) rather than letting `quantile()`
# choke on `numeric(0)`.
.boot_ci <- function(mat, level) {
  a <- (1 - level) / 2
  q <- function(x, p) if (all(is.na(x))) NA_real_ else
    stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  list(low  = apply(mat, 1, q, p = a),
       high = apply(mat, 1, q, p = 1 - a))
}

# Pick the target topic: an explicit `topic`, or the topic whose top-20 words best
# overlap `anchor_words`. All-zero overlap silently resolves to topic 1 via
# `which.max`, so warn when that happens (the point-estimate call; muted in the
# bootstrap loop via `warn = FALSE`).
.pick_topic <- function(lb, vocab, topic, anchor_words, warn = TRUE) {
  if (!is.null(anchor_words)) {
    avg <- Reduce(`+`, lb) / length(lb)
    ov <- apply(avg, 1, function(r)
      sum(vocab[order(r, decreasing = TRUE)[1:20]] %in% anchor_words))
    if (warn && max(ov) == 0)
      warning("no `anchor_words` matched any topic's top-20 words; defaulting to ",
              "topic 1 -- check `anchor_words` against the model vocabulary.",
              call. = FALSE)
    which.max(ov)
  } else if (!is.null(topic)) {
    as.integer(topic)
  } else {
    stop("supply either `topic` or `anchor_words`.", call. = FALSE)
  }
}

# Resolve the two content groups to contrast, guarding the single-group case (a
# `content_time` model with no `content` covariate has one base level, so the
# default second group would be NA -> silent all-NA output).
.resolve_groups <- function(groups, base) {
  if (!is.null(groups)) return(groups)
  ub <- unique(base)
  if (length(ub) < 2L)
    stop("the model has a single content group ('", ub[1], "'); ",
         "content trajectories/divergence compare two groups. Fit with a `content` ",
         "covariate, or pass `groups` explicitly.", call. = FALSE)
  ub[1:2]
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
.content_div_from_fit <- function(fit, groups, topic, anchor_words, measure,
                                  warn = TRUE) {
  lb <- fit$beta$logbeta
  lev <- fit$settings$covariates$yvarlevels
  if (is.null(lev) || !any(grepl("@", lev)))
    stop("`object` was not fit with a content_time covariate.", call. = FALSE)
  parts <- do.call(rbind, strsplit(lev, "@", fixed = TRUE))
  base <- parts[, 1]; per <- parts[, 2]
  groups <- .resolve_groups(groups, base)
  # `unique(per)` follows the fit's cell order (base-major, period-minor), which is
  # the chronological factor-level order the RW smoother tied -- do NOT re-sort, or
  # ordered factors like c("pre","during","post") come out of sequence.
  periods <- unique(per)
  vocab <- fit$vocab
  k <- .pick_topic(lb, vocab, topic, anchor_words, warn = warn)

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
.content_traj_from_fit <- function(fit, words, groups, topic, anchor_words,
                                   warn = TRUE) {
  lb <- fit$beta$logbeta
  lev <- fit$settings$covariates$yvarlevels
  if (is.null(lev) || !any(grepl("@", lev)))
    stop("`object` was not fit with a content_time covariate ",
         "(no 'group@period' content levels found).", call. = FALSE)
  vocab <- fit$vocab
  present <- intersect(words, vocab)
  if (length(present) == 0L)
    stop("none of `words` are in the model vocabulary.", call. = FALSE)
  parts <- do.call(rbind, strsplit(lev, "@", fixed = TRUE))
  base <- parts[, 1]; per <- parts[, 2]
  groups <- .resolve_groups(groups, base)
  # `unique(per)` follows the fit's cell order (chronological factor-level order the
  # RW smoother tied); re-sorting would misorder ordered-factor periods.
  periods <- unique(per)
  k <- .pick_topic(lb, vocab, topic, anchor_words, warn = warn)

  cell <- function(g, p) match(paste0(g, "@", p), lev)
  out <- do.call(rbind, lapply(present, function(w) {
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
