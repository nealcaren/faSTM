# Less-common stm-parity functions: permutation test for a binary covariate,
# topic->outcome lasso, cross-run stability, and LDA-C corpus IO.

# Align the topics of `b_new` (K x V) to `b_ref`: returns a permutation `p` with
# b_new[p[k], ] matching b_ref[k, ]. Uses clue's optimal assignment when
# available, else a greedy cosine match.
.align_topics <- function(b_ref, b_new) {
  rn <- b_ref / sqrt(rowSums(b_ref^2)); nn <- b_new / sqrt(rowSums(b_new^2))
  sim <- rn %*% t(nn)                                  # K x K cosine
  if (requireNamespace("clue", quietly = TRUE)) {
    as.integer(clue::solve_LSAP(sim, maximum = TRUE))
  } else {
    K <- nrow(sim); p <- integer(K); used <- logical(K)
    for (i in order(-apply(sim, 1L, max))) {
      j <- which.max(ifelse(used, -Inf, sim[i, ])); p[i] <- j; used[j] <- TRUE
    }
    p
  }
}

# Treatment coefficient per topic from an estimateEffect-style fit.
.treatment_effects <- function(formula, model, meta, treatment, K) {
  rhs <- paste(attr(stats::terms(formula), "term.labels"), collapse = " + ")
  ef <- stats::as.formula(paste0("1:", K, " ~ ", rhs))
  eff <- estimateEffect(ef, model, metadata = meta, uncertainty = "None")
  j <- match(treatment, eff$terms)
  vapply(seq_len(K), function(k) {
    if (is.na(j)) NA_real_ else eff$coefficients[[paste0("topic", k)]]$est[j]
  }, numeric(1))
}

#' Permutation test for a binary covariate's effect on topics
#'
#' Refits the model many times with the treatment labels permuted, aligning
#' topics across refits, to build a null distribution for the treatment effect
#' on each topic (cf. `stm::permutationTest`). Fast because each refit is cheap.
#'
#' @param formula Prevalence formula whose RHS includes `treatment`.
#' @param model A faSTM fit.
#' @param treatment Name of a 0/1 covariate in `corpus$meta`.
#' @param corpus The `faSTM_corpus` the model was fit on.
#' @param nruns Total models (1 reference + `nruns-1` permutations).
#' @param seed RNG seed.
#' @param ... Passed to [stm()] for the refits.
#' @return A `faSTM_permtest` with `ref` (observed per-topic effects) and `null`
#'   (`(nruns-1)` × K permuted effects).
#' @export
permutation_test <- function(formula, model, treatment, corpus, nruns = 100L,
                             seed = NULL, ...) {
  meta <- corpus$meta
  if (!treatment %in% names(meta)) stop("`treatment` not in corpus metadata.", call. = FALSE)
  if (!all(meta[[treatment]] %in% c(0, 1))) stop("`treatment` must be binary 0/1.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  K <- ncol(model$theta)
  prevf <- stats::reformulate(attr(stats::terms(formula), "term.labels"))
  b_ref <- exp(model$beta$logbeta[[1]])
  prob <- mean(meta[[treatment]])

  ref <- .treatment_effects(formula, model, meta, treatment, K)
  null <- matrix(NA_real_, nruns - 1L, K)
  for (i in seq_len(nruns - 1L)) {
    m2 <- meta; m2[[treatment]] <- stats::rbinom(nrow(meta), 1L, prob)
    fit2 <- stm(corpus, K = K, prevalence = prevf, data = m2, verbose = FALSE, ...)
    p <- .align_topics(b_ref, exp(fit2$beta$logbeta[[1]]))   # fit2 topic for each ref topic
    eff2 <- .treatment_effects(formula, fit2, m2, treatment, K)
    null[i, ] <- eff2[p]
  }
  structure(list(ref = ref, null = null, treatment = treatment, topics = seq_len(K)),
            class = "faSTM_permtest")
}

#' @export
print.faSTM_permtest <- function(x, ...) {
  pv <- vapply(x$topics, function(k)
    mean(abs(x$null[, k]) >= abs(x$ref[k]), na.rm = TRUE), numeric(1))
  cat("faSTM permutation_test on '", x$treatment, "' (", nrow(x$null) + 1L, " runs)\n", sep = "")
  print(data.frame(topic = x$topics, effect = round(x$ref, 4),
                   perm_p = round(pv, 3)), row.names = FALSE)
  invisible(x)
}

#' Predict a document-level outcome from topic proportions (lasso)
#'
#' Cross-validated lasso (`glmnet`) of an outcome on the topic-proportion matrix
#' (cf. `stm::topicLasso`). Identifies which topics predict the outcome.
#'
#' @param formula `outcome ~ .` — the LHS names the outcome in `data`.
#' @param model A faSTM fit (supplies the topic proportions).
#' @param data Document-level data with the outcome, aligned to the documents.
#' @param family glmnet family (`"gaussian"`, `"binomial"`, ...).
#' @param nfolds CV folds.
#' @param seed RNG seed.
#' @param ... Passed to [glmnet::cv.glmnet()].
#' @return A `faSTM_topiclasso` with selected per-topic coefficients.
#' @export
topic_lasso <- function(formula, model, data, family = "gaussian",
                        nfolds = 10L, seed = 2138L, ...) {
  if (!requireNamespace("glmnet", quietly = TRUE))
    stop("topic_lasso() needs the 'glmnet' package.", call. = FALSE)
  y <- eval(formula[[2L]], envir = data)
  X <- model$theta
  set.seed(seed)
  cv <- glmnet::cv.glmnet(X, y, family = family, nfolds = nfolds, ...)
  co <- as.numeric(stats::coef(cv, s = "lambda.min"))[-1L]   # drop intercept
  labels <- apply(label_topics(model, n = 3L)$prob, 1L, paste, collapse = ", ")
  structure(list(coefficients = co, labels = labels, topics = seq_along(co), cv = cv),
            class = "faSTM_topiclasso")
}

#' @export
print.faSTM_topiclasso <- function(x, ...) {
  sel <- which(x$coefficients != 0)
  cat("faSTM topic_lasso — topics selected:", length(sel), "of", length(x$coefficients), "\n")
  if (length(sel))
    print(data.frame(topic = sel, coef = round(x$coefficients[sel], 4),
                     words = x$labels[sel]), row.names = FALSE)
  invisible(x)
}

#' Cross-run topic stability
#'
#' Aligns every model from a [select_model()] run to the first and reports how
#' stable each topic's top words are across runs (cf. `stm::multiSTM`).
#'
#' @param x A `faSTM_selectmodel`.
#' @param n Top words used for the stability score.
#' @return A `faSTM_multistm` with a per-topic mean top-word agreement.
#' @export
multi_stm <- function(x, n = 10L) {
  stopifnot(inherits(x, "faSTM_selectmodel"))
  models <- x$models
  ref <- models[[1]]; b_ref <- exp(ref$beta$logbeta[[1]]); K <- nrow(b_ref)
  ref_top <- apply(b_ref, 1L, function(b) order(-b)[seq_len(n)])
  agree <- matrix(NA_real_, length(models) - 1L, K)
  for (i in seq_along(models)[-1]) {
    b <- exp(models[[i]]$beta$logbeta[[1]]); p <- .align_topics(b_ref, b)
    top <- apply(b, 1L, function(z) order(-z)[seq_len(n)])
    for (k in seq_len(K))
      agree[i - 1L, k] <- length(intersect(ref_top[, k], top[, p[k]])) / n
  }
  structure(list(stability = colMeans(agree), overall = mean(agree),
                 nmodels = length(models)),
            class = "faSTM_multistm")
}

#' @export
print.faSTM_multistm <- function(x, ...) {
  cat(sprintf("faSTM multi_stm — %d models, mean top-word stability %.2f\n",
              x$nmodels, x$overall))
  print(data.frame(topic = seq_along(x$stability), stability = round(x$stability, 2)),
        row.names = FALSE)
  invisible(x)
}

#' Read/write a corpus in LDA-C (Blei) sparse format
#'
#' Each line is `M term:count term:count ...` with 0-based term ids.
#'
#' @param file Path to the `.ldac`/`.dat` file (read) or output path (write).
#' @return `read_ldac` returns a list of 2×n integer matrices (faSTM/stm document
#'   format, 1-based ids); `write_ldac` returns the path invisibly.
#' @export
read_ldac <- function(file) {
  lines <- readLines(file)
  lapply(lines[nzchar(lines)], function(ln) {
    parts <- strsplit(trimws(ln), "\\s+")[[1]][-1L]    # drop leading M
    if (length(parts) == 0) return(matrix(integer(0), nrow = 2L))
    tc <- do.call(rbind, lapply(strsplit(parts, ":"), as.integer))
    matrix(as.integer(rbind(tc[, 1] + 1L, tc[, 2])), nrow = 2L)  # -> 1-based
  })
}

#' @rdname read_ldac
#' @param documents A list of 2×n integer matrices (1-based ids).
#' @export
write_ldac <- function(documents, file) {
  con <- file(file, "w"); on.exit(close(con))
  for (m in documents) {
    if (ncol(m) == 0L) { writeLines("0", con); next }
    writeLines(paste(ncol(m), paste(sprintf("%d:%d", m[1L, ] - 1L, m[2L, ]),
                                    collapse = " ")), con)
  }
  invisible(file)
}
