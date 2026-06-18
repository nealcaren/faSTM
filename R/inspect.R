# Model inspection: top words, FREX/lift/score, representative docs, coherence,
# exclusivity, topic correlations. Pure R over the fitted object. Formulas are
# faithful ports of stm's calcfrex / calclift / calcscore / exclusivity /
# semCoh1beta so the numbers match what reviewers expect.

.lse_cols <- function(logbeta) {           # column log-sum-exp (over topics)
  apply(logbeta, 2L, function(c) { m <- max(c); m + log(sum(exp(c - m))) })
}

#' FREX scores for every word and topic
#'
#' FREX balances word *frequency* and *exclusivity* (Bischof & Airoldi 2012;
#' Roberts et al.). Unlike `stm`'s `labelTopics()`, this returns the full numeric
#' FREX matrix, not just the ranked words (addresses a long-standing `stm`
#' request, bstewart/stm#265).
#'
#' @param model A faSTM fit.
#' @param w FREX frequency/exclusivity weight (0.5 = equal).
#' @return A topics × vocabulary matrix of FREX scores (columns named by vocab).
#' @export
frex_scores <- function(model, w = 0.5) {
  logbeta <- model$beta$logbeta[[1]]                 # K x V
  excl <- logbeta - matrix(.lse_cols(logbeta), nrow(logbeta), ncol(logbeta), byrow = TRUE)
  V <- ncol(logbeta)
  freqscore <- t(apply(logbeta, 1L, rank)) / V       # K x V
  exclscore <- t(apply(excl,    1L, rank)) / V
  frex <- 1 / (w / freqscore + (1 - w) / exclscore)
  colnames(frex) <- model$vocab
  frex
}

#' Label topics by top words (prob, FREX, lift, score)
#'
#' @param model A faSTM fit.
#' @param n Number of words per topic per metric.
#' @param frexweight FREX frequency/exclusivity weight.
#' @return A `faSTM_labels` object: per-metric top-word matrices (`prob`,
#'   `frex`, `lift`, `score`), each topics × `n`.
#' @export
label_topics <- function(model, n = 7L, frexweight = 0.5) {
  logbeta <- model$beta$logbeta[[1]]; K <- nrow(logbeta); V <- ncol(logbeta)
  vocab <- model$vocab; wc <- model$word_counts
  topw <- function(scoremat) t(apply(scoremat, 1L, function(s) vocab[order(-s)[seq_len(n)]]))

  prob <- topw(logbeta)
  frex <- topw(frex_scores(model, w = frexweight))
  emp  <- log(wc) - log(sum(wc))
  lift <- topw(logbeta - matrix(emp, K, V, byrow = TRUE))
  score <- topw(exp(logbeta) * (logbeta - matrix(colMeans(logbeta), K, V, byrow = TRUE)))

  structure(list(prob = prob, frex = frex, lift = lift, score = score,
                 topics = seq_len(K)),
            class = "faSTM_labels")
}

#' @export
print.faSTM_labels <- function(x, ...) {
  for (k in x$topics) {
    cat(sprintf("Topic %d:\n", k))
    cat("  Highest Prob:", paste(x$prob[k, ], collapse = ", "), "\n")
    cat("  FREX:        ", paste(x$frex[k, ], collapse = ", "), "\n")
    cat("  Lift:        ", paste(x$lift[k, ], collapse = ", "), "\n")
    cat("  Score:       ", paste(x$score[k, ], collapse = ", "), "\n")
  }
  invisible(x)
}

#' Representative documents for each topic
#'
#' @param model A faSTM fit.
#' @param texts Optional character vector of the raw document texts, aligned to
#'   the fitted documents; returned alongside the indices when supplied.
#' @param topics Topics to report (default all).
#' @param n Documents per topic.
#' @return A list with `index` (per-topic document indices) and, if `texts` is
#'   given, `docs` (the texts).
#' @export
find_thoughts <- function(model, texts = NULL, topics = NULL, n = 3L) {
  theta <- model$theta; K <- ncol(theta)
  topics <- if (is.null(topics)) seq_len(K) else topics
  idx <- lapply(topics, function(k) order(-theta[, k])[seq_len(n)])
  names(idx) <- paste0("Topic ", topics)
  out <- list(index = idx)
  if (!is.null(texts)) out$docs <- lapply(idx, function(ii) texts[ii])
  structure(out, class = "faSTM_thoughts")
}

#' Semantic coherence (Mimno et al. 2011)
#'
#' Sum over the top-`M` words of each topic of `log((D(w_i,w_j)+1)/D(w_j))`,
#' using document co-occurrence counts. Higher (less negative) is more coherent.
#'
#' @param model A faSTM fit (must carry its document-term matrix; faSTM stores it).
#' @param M Number of top words per topic.
#' @return A numeric vector, one coherence value per topic.
#' @export
semantic_coherence <- function(model, M = 10L) {
  dtm <- .require_dtm(model)
  logbeta <- model$beta$logbeta[[1]]; K <- nrow(logbeta)
  ## Replicate stm::semCoh1beta exactly: pair direction is by GLOBAL wordlist
  ## index (the unique union of every topic's top words), not within-topic rank,
  ## so the denominator is the smaller-index word's marginal.
  topw <- apply(logbeta, 1L, function(b) order(-b)[seq_len(M)])  # M x K
  wordlist <- unique(as.vector(topw))
  inc <- methods::as(dtm[, wordlist, drop = FALSE] > 0, "CsparseMatrix")
  cross <- as.matrix(Matrix::crossprod(inc))            # |wordlist|^2 co-doc counts
  pos <- matrix(match(as.vector(topw), wordlist), nrow = M)  # topw -> wordlist index
  coh <- numeric(K)
  for (k in seq_len(K)) {
    idx <- pos[, k]; s <- 0
    for (a in seq_len(M)) for (b in seq_len(M)) if (idx[a] > idx[b])
      s <- s + log(0.01 + cross[idx[a], idx[b]]) - log(0.01 + cross[idx[b], idx[b]])
    coh[k] <- s
  }
  coh
}

#' Topic exclusivity (FREX-summary, frexw default 0.7)
#'
#' @param model A faSTM fit.
#' @param M Top words per topic.
#' @param frexw Frequency/exclusivity weight.
#' @return A numeric vector, one exclusivity value per topic.
#' @export
exclusivity <- function(model, M = 10L, frexw = 0.7) {
  tbeta <- t(exp(model$beta$logbeta[[1]]))             # V x K
  mat <- tbeta / rowSums(tbeta)
  ex <- apply(mat, 2L, rank) / nrow(mat)
  fr <- apply(tbeta, 2L, rank) / nrow(mat)
  frex <- 1 / (frexw / ex + (1 - frexw) / fr)
  idx <- apply(tbeta, 2L, order, decreasing = TRUE)[seq_len(M), ]
  vapply(seq_len(ncol(frex)), function(i) sum(frex[idx[, i], i]), numeric(1))
}

#' Topic correlation graph (positive correlations of topic proportions)
#'
#' @param model A faSTM fit.
#' @param cutoff Correlation threshold for an edge.
#' @return A list with `cor` (the K×K correlation matrix) and `posadj` (the
#'   thresholded positive adjacency).
#' @export
topic_correlation <- function(model, cutoff = 0.01) {
  cmat <- stats::cor(model$theta)
  posadj <- (cmat > cutoff) * 1L; diag(posadj) <- 0L
  list(cor = cmat, posadj = posadj)
}

#' Labels for a content (SAGE) model
#'
#' For models fit with a `content` covariate, reports each topic's marginal top
#' words plus, for every content group, the words most distinctive to that group
#' within the topic (group-vs-marginal log-ratio — the SAGE deviation).
#'
#' @param model A faSTM fit with a content covariate.
#' @param n Words per list.
#' @return A `faSTM_sagelabels` object.
#' @export
sage_labels <- function(model, n = 7L) {
  lb <- model$beta$logbeta
  if (length(lb) < 2L)
    stop("sage_labels() needs a content model (fit stm(..., content = ~ group)).",
         call. = FALSE)
  vocab <- model$vocab; K <- nrow(lb[[1]]); G <- length(lb)
  groups <- model$settings$covariates$yvarlevels
  if (is.null(groups)) groups <- paste0("group", seq_len(G))
  marg <- Reduce(`+`, lapply(lb, exp)) / G            # marginal topic-word
  topw <- function(s) vocab[order(-s)[seq_len(n)]]
  marginal <- t(apply(marg, 1L, topw))
  bygroup <- lapply(seq_len(G), function(g)
    t(vapply(seq_len(K), function(k) topw(lb[[g]][k, ] - log(marg[k, ])), character(n))))
  names(bygroup) <- groups
  structure(list(marginal = marginal, bygroup = bygroup, groups = groups,
                 topics = seq_len(K)),
            class = "faSTM_sagelabels")
}

#' @export
print.faSTM_sagelabels <- function(x, ...) {
  for (k in x$topics) {
    cat(sprintf("Topic %d:\n", k))
    cat("  Marginal:", paste(x$marginal[k, ], collapse = ", "), "\n")
    for (g in x$groups)
      cat(sprintf("  %s:%s %s\n", g, strrep(" ", max(0, 8 - nchar(g))),
                  paste(x$bygroup[[g]][k, ], collapse = ", ")))
  }
  invisible(x)
}

.require_dtm <- function(model) {
  if (is.null(model$dtm))
    stop("this fit has no stored document-term matrix; refit with stm() on a ",
         "faSTM corpus/dfm (not a bare stm documents list).", call. = FALSE)
  model$dtm
}
