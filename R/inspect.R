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
  K <- nrow(logbeta); V <- ncol(logbeta)
  excl <- logbeta - matrix(.lse_cols(logbeta), K, V, byrow = TRUE)  # log p(topic|word)
  ## With corpus word counts available, stm James-Stein-shrinks each word's
  ## topic-exclusivity toward uniform before ranking (stm:::js.estimate); match it.
  wc <- model$word_counts
  if (!is.null(wc)) {
    ep <- exp(excl)                                  # K x V, columns ~ sum to 1
    ep <- vapply(seq_len(V), function(v) .js_estimate(ep[, v], wc[v]), numeric(K))
    excl <- log(pmax(ep, .Machine$double.eps))       # safelog
  }
  freqscore <- t(apply(logbeta, 1L, rank)) / V       # K x V
  exclscore <- t(apply(excl,    1L, rank)) / V
  frex <- 1 / (w / freqscore + (1 - w) / exclscore)
  colnames(frex) <- model$vocab
  frex
}

# stm:::js.estimate — James-Stein shrinkage of a probability vector toward uniform.
.js_estimate <- function(prob, ct) {
  n <- length(prob); unif <- rep(1 / n, n)
  if (ct <= 1) return(unif)
  mlvar <- prob * (1 - prob) / (ct - 1)
  dev <- sum((prob - unif)^2)
  if (dev == 0) return(prob)
  lambda <- sum(mlvar) / dev
  if (is.nan(lambda)) return(unif)
  lambda <- min(max(lambda, 0), 1)
  lambda * unif + (1 - lambda) * prob
}

#' Marginal content words by one content covariate
#'
#' For a multi-covariate (crossed) content model, recovers the topic-word labels
#' for each level of a single content covariate, averaging the crossed
#' topic-word distributions over the other covariate(s). Lets you read off how
#' topics' vocabulary shifts with one covariate while marginalizing the rest.
#'
#' @param model A content (SAGE) faSTM fit.
#' @param by Content covariate name to marginalize *to* (default: the first).
#' @param n Words per topic.
#' @param type `"prob"`, `"lift"`, or `"frex"`.
#' @return A named list (one entry per level of `by`) of K x `n` word matrices.
#' @export
content_topics <- function(model, by = NULL, n = 7L,
                           type = c("prob", "lift", "frex")) {
  type <- match.arg(type)
  lb <- model$beta$logbeta
  if (length(lb) < 2L)
    stop("content_topics() needs a content model (stm(..., content = ~ ...)).", call. = FALSE)
  gt <- model$settings$covariates$contenttable
  vars <- model$settings$covariates$contentvars
  if (is.null(gt) || is.null(vars))
    stop("this fit lacks content covariate metadata; refit with faSTM >= this version.",
         call. = FALSE)
  if (is.null(by)) by <- vars[1L]
  if (!by %in% vars) stop("`by` must be one of: ", paste(vars, collapse = ", "), call. = FALSE)

  vocab <- model$vocab; K <- nrow(lb[[1]]); V <- ncol(lb[[1]]); wc <- model$word_counts
  bylevels <- unique(gt[[by]])
  scorer <- function(bp) {                      # bp: K x V probabilities
    lg <- log(bp)
    switch(type,
      prob = lg,
      lift = lg - matrix(log(wc) - log(sum(wc)), K, V, byrow = TRUE),
      frex = {
        excl <- lg - matrix(.lse_cols(lg), K, V, byrow = TRUE)
        1 / (0.5 / (t(apply(lg, 1L, rank)) / V) + 0.5 / (t(apply(excl, 1L, rank)) / V))
      })
  }
  out <- lapply(bylevels, function(l) {
    gs <- gt$group[gt[[by]] == l]                                   # crossed groups w/ this level
    bp <- Reduce(`+`, lapply(gs, function(g) exp(lb[[g]]))) / length(gs)
    sc <- scorer(bp)
    t(apply(sc, 1L, function(r) vocab[order(-r)[seq_len(n)]]))      # K x n
  })
  names(out) <- bylevels
  out
}

#' Expected topic proportions (the numbers behind the summary plot)
#'
#' Returns the corpus-level expected topic proportions — the mean of theta per
#' topic — as a numeric table, so you can read off the values stm's
#' `plot(type = "summary")` displays (stm issue #269).
#'
#' @param model A faSTM fit.
#' @param nlabel Top FREX words to attach as a topic label.
#' @return A data.frame with `topic`, `proportion`, `label`, sorted by proportion.
#' @export
topic_proportions <- function(model, nlabel = 3L) {
  prop <- colMeans(model$theta)
  lab <- apply(label_topics(model, n = nlabel)[["frex"]], 1L, paste, collapse = ", ")
  out <- data.frame(topic = seq_along(prop), proportion = prop, label = lab,
                    stringsAsFactors = FALSE)
  out[order(-out$proportion), , drop = FALSE]
}

#' Top terms per topic, with their numeric scores (tidy)
#'
#' Like [label_topics()] but returns the *values* behind the ranking, not just
#' the words — e.g. the numeric FREX score per top term (stm issue #265).
#'
#' @param model A faSTM fit.
#' @param n Terms per topic.
#' @param by Ranking measure: `"prob"`, `"frex"`, `"lift"`, or `"score"`.
#' @param frexweight FREX frequency/exclusivity weight (used when `by = "frex"`).
#' @return A tidy data.frame with `topic`, `rank`, `term`, `score`, `measure`.
#' @export
topic_terms <- function(model, n = 7L, by = c("prob", "frex", "lift", "score"),
                        frexweight = 0.5) {
  by <- match.arg(by)
  logbeta <- model$beta$logbeta[[1]]; K <- nrow(logbeta); V <- ncol(logbeta)
  vocab <- model$vocab; wc <- model$word_counts
  scoremat <- switch(by,
    prob  = logbeta,
    frex  = frex_scores(model, w = frexweight),
    lift  = logbeta - matrix(log(wc) - log(sum(wc)), K, V, byrow = TRUE),
    score = exp(logbeta) * (logbeta - matrix(colMeans(logbeta), K, V, byrow = TRUE)))
  do.call(rbind, lapply(seq_len(K), function(k) {
    ord <- order(-scoremat[k, ])[seq_len(n)]
    data.frame(topic = k, rank = seq_len(n), term = vocab[ord],
               score = scoremat[k, ord], measure = by,
               stringsAsFactors = FALSE)
  }))
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

  ## stm's labelTopics matrices carry NULL dimnames; strip ours so a direct
  ## unclass() comparison against stm matches (values already do).
  bare <- function(m) { dimnames(m) <- NULL; m }
  prob <- bare(topw(logbeta))
  frex <- bare(topw(frex_scores(model, w = frexweight)))
  emp  <- log(wc) - log(sum(wc))
  lift <- bare(topw(logbeta - matrix(emp, K, V, byrow = TRUE)))
  score <- bare(topw(exp(logbeta) * (logbeta - matrix(colMeans(logbeta), K, V, byrow = TRUE))))

  structure(list(prob = prob, frex = frex, lift = lift, score = score,
                 topics = seq_len(K), topicnums = seq_len(K)),
            class = "faSTM_labels")
}

#' @export
print.faSTM_labels <- function(x, ...) {
  for (i in seq_along(x$topics)) {
    cat(sprintf("Topic %d:\n", x$topics[i]))
    cat("  Highest Prob:", paste(x$prob[i, ], collapse = ", "), "\n")
    cat("  FREX:        ", paste(x$frex[i, ], collapse = ", "), "\n")
    cat("  Lift:        ", paste(x$lift[i, ], collapse = ", "), "\n")
    cat("  Score:       ", paste(x$score[i, ], collapse = ", "), "\n")
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
sage_labels <- function(model, n = 7L, frexweight = NULL) {
  lb <- model$beta$logbeta
  if (length(lb) < 2L)
    stop("sage_labels() needs a content model (fit stm(..., content = ~ group)).",
         call. = FALSE)
  vocab <- model$vocab; K <- nrow(lb[[1]]); G <- length(lb)
  V <- ncol(lb[[1]])
  groups <- model$settings$covariates$yvarlevels
  if (is.null(groups)) groups <- paste0("group", seq_len(G))
  marg <- Reduce(`+`, lapply(lb, exp)) / G            # marginal topic-word
  topw <- function(s) vocab[order(-s)[seq_len(n)]]
  marginal <- t(apply(marg, 1L, topw))
  ## Per group, rank words for each topic. Default: the group-vs-marginal log
  ## ratio (how much the group emphasizes a word). With `frexweight` set, blend
  ## that exclusivity with the group's frequency, FREX-style (stm issue #189).
  group_score <- function(g, k) {
    excl <- lb[[g]][k, ] - log(marg[k, ])
    if (is.null(frexweight)) return(excl)
    freqr <- rank(lb[[g]][k, ]) / V
    exclr <- rank(excl) / V
    1 / (frexweight / freqr + (1 - frexweight) / exclr)
  }
  bygroup <- lapply(seq_len(G), function(g)
    t(vapply(seq_len(K), function(k) topw(group_score(g, k)), character(n))))
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

#' Topic coherence (Mimno / NPMI / c_v)
#'
#' Coherence scores for each topic's top-`M` words, computed from the fit's
#' stored document-term matrix. `"mimno"` is the UMass-style score of
#' [semantic_coherence()]; `"npmi"` averages pairwise normalized PMI; `"c_v"` is
#' the Roeder et al. (2015) measure (one-set segmentation, NPMI confirmation,
#' cosine aggregation). NPMI/c_v use *document* co-occurrence as the probability
#' estimator. Higher is more coherent (npmi/c_v are roughly in [-1, 1]).
#'
#' @param model A faSTM fit (carries its DTM).
#' @param measure `"mimno"`, `"npmi"`, or `"c_v"`.
#' @param M Top words per topic.
#' @return A numeric vector, one coherence score per topic.
#' @export
coherence <- function(model, measure = c("mimno", "npmi", "c_v"), M = 10L) {
  measure <- match.arg(measure)
  if (measure == "mimno") return(semantic_coherence(model, M = M))
  dtm <- model$dtm
  if (is.null(dtm)) stop("coherence() needs the fit's stored DTM.", call. = FALSE)
  pres <- methods::as(dtm > 0, "CsparseMatrix")      # D x V binary presence
  nD <- nrow(pres)
  logbeta <- model$beta$logbeta[[1]]; K <- nrow(logbeta); V <- ncol(logbeta)
  M <- min(M, V)
  npmi_pair <- function(co, di, a, b) {
    pij <- co[a, b] / nD
    if (pij <= 0) return(if (measure == "npmi") -1 else 0)   # never co-occur
    denom <- -log(pij)
    if (denom < 1e-12) return(1)                              # pij ~ 1: perfect co-occurrence
    val <- log(pij / ((di[a] / nD) * (di[b] / nD))) / denom
    if (!is.finite(val)) 0 else max(-1, min(1, val))
  }
  vapply(seq_len(K), function(k) {
    idx <- order(-logbeta[k, ])[seq_len(M)]
    co  <- as.matrix(crossprod(pres[, idx, drop = FALSE]))   # M x M co-doc counts
    di  <- diag(co)
    if (measure == "npmi") {
      pr <- utils::combn(M, 2L)
      mean(vapply(seq_len(ncol(pr)), function(j) npmi_pair(co, di, pr[1, j], pr[2, j]), numeric(1)))
    } else {                                                  # c_v
      N <- matrix(0, M, M)
      for (a in seq_len(M)) for (b in seq_len(M))
        N[a, b] <- if (a == b) 1 else npmi_pair(co, di, a, b)
      s <- colSums(N)
      mean(vapply(seq_len(M), function(i)
        sum(N[i, ] * s) / (sqrt(sum(N[i, ]^2)) * sqrt(sum(s^2)) + 1e-12), numeric(1)))
    }
  }, numeric(1))
}
