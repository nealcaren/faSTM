# Modern (ggplot2) plotting. Every function returns a ggplot object the caller
# can theme/extend. ggplot2 is a Suggests dependency, loaded on demand.

.need_ggplot <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("plotting needs the 'ggplot2' package: install.packages('ggplot2').", call. = FALSE)
}

#' Plot search_k diagnostics
#'
#' Faceted held-out likelihood, semantic coherence, exclusivity and bound vs K.
#' @param x A `faSTM_searchk`.
#' @param ... Unused.
#' @return A ggplot object.
#' @exportS3Method plot faSTM_searchk
plot.faSTM_searchk <- function(x, ...) {
  .need_ggplot()
  long <- as.data.frame(x)
  pretty <- c(heldout = "held-out likelihood", semcoh = "semantic coherence",
              exclusivity = "exclusivity", residual = "residual dispersion",
              bound = "bound")
  long$metric <- factor(pretty[long$metric], levels = pretty[unique(long$metric)])
  ggplot2::ggplot(long, ggplot2::aes(.data$K, .data$value)) +
    ggplot2::geom_line(color = "grey55") +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ metric, scales = "free_y") +
    ggplot2::labs(x = "number of topics (K)", y = NULL,
                  title = "Choosing K") +
    ggplot2::theme_minimal(base_size = 12)
}

#' Plot a fitted model
#'
#' @param x A faSTM fit.
#' @param type `"summary"` — topics ranked by expected corpus prevalence, labelled
#'   with their top words.
#' @param n Top words to label each topic with.
#' @param labeltype Word ranking for labels: `"frex"`, `"prob"`, `"lift"`, `"score"`.
#' @param ... Unused.
#' @return A ggplot object.
#' @exportS3Method plot faSTM
plot.faSTM <- function(x, type = "summary", n = 5L, labeltype = "frex", ...) {
  .need_ggplot()
  type <- match.arg(type, "summary")
  prop <- colMeans(x$theta)
  words <- apply(label_topics(x, n = n)[[labeltype]], 1L, paste, collapse = ", ")
  df <- data.frame(topic = factor(seq_along(prop)), prop = prop, words = words)
  df <- df[order(df$prop), ]
  df$topic <- factor(df$topic, levels = df$topic)
  ggplot2::ggplot(df, ggplot2::aes(.data$prop, .data$topic)) +
    ggplot2::geom_col(fill = "grey80", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(x = 0, label = paste0("  ", words)),
                       hjust = 0, size = 3.4) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "expected topic proportion", y = NULL,
                  title = "Topics by prevalence",
                  subtitle = paste("labels:", labeltype)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot estimated covariate effects on topic prevalence
#'
#' @param x A `faSTM_effect` (from [estimateEffect()]).
#' @param covariate Name of the covariate to vary.
#' @param method `"pointestimate"` (mean proportion per level of a categorical
#'   covariate), `"continuous"` (proportion vs a numeric covariate, with ribbon),
#'   or `"difference"` (difference between two `values`).
#' @param topics Topics to show (default all in the effect object).
#' @param values For `"difference"`, length-2 `c(high, low)`; for `"continuous"`,
#'   optional range; ignored for `"pointestimate"`.
#' @param npoints Grid size for `"continuous"`.
#' @param ci Confidence level.
#' @param ... Unused.
#' @return A ggplot object.
#' @exportS3Method plot faSTM_effect
plot.faSTM_effect <- function(x, covariate, method = c("pointestimate", "continuous", "difference"),
                              topics = x$topics, values = NULL, npoints = 50L, ci = 0.95, ...) {
  .need_ggplot()
  method <- match.arg(method)
  meta <- x$metadata
  if (!covariate %in% names(meta)) stop("`covariate` not found in the effect metadata.", call. = FALSE)
  is_factor <- is.factor(meta[[covariate]]) || is.character(meta[[covariate]])

  grid <- switch(method,
    pointestimate = { if (!is_factor) stop("pointestimate is for categorical covariates.", call. = FALSE)
                      levels(as.factor(meta[[covariate]])) },
    difference    = { if (is.null(values) || length(values) != 2) stop("difference needs values = c(high, low).", call. = FALSE)
                      values },
    continuous    = { if (is_factor) stop("continuous is for numeric covariates.", call. = FALSE)
                      rng <- if (is.null(values)) range(meta[[covariate]]) else range(values)
                      seq(rng[1], rng[2], length.out = npoints) })

  preds <- .effect_grid(x, covariate, grid, topics)        # est, se per (topic, value)
  z <- stats::qt(1 - (1 - ci) / 2, df = x$coefficients[[1]]$df)
  preds$lower <- preds$est - z * preds$se
  preds$upper <- preds$est + z * preds$se
  preds$topic <- factor(paste("Topic", preds$topic),
                        levels = paste("Topic", topics))

  if (method == "continuous") {
    preds$x <- as.numeric(preds$value)
    ggplot2::ggplot(preds, ggplot2::aes(.data$x, .data$est)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                           alpha = 0.18, fill = "#2c7fb8") +
      ggplot2::geom_line(color = "#2c7fb8", linewidth = 0.8) +
      ggplot2::facet_wrap(~ topic, scales = "free_y") +
      ggplot2::labs(x = covariate, y = "expected topic proportion",
                    title = paste("Effect of", covariate)) +
      ggplot2::theme_minimal(base_size = 12)
  } else {
    if (method == "difference") {
      d <- .effect_difference(x, covariate, values, topics)
      d$lower <- d$est - z * d$se; d$upper <- d$est + z * d$se
      d$topic <- factor(paste("Topic", d$topic), levels = paste("Topic", rev(topics)))
      ggplot2::ggplot(d, ggplot2::aes(.data$est, .data$topic)) +
        ggplot2::geom_vline(xintercept = 0, linetype = 2, color = "grey60") +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                               orientation = "y", width = 0.25) +
        ggplot2::geom_point(size = 2.4) +
        ggplot2::labs(x = sprintf("difference in proportion (%s = %s vs %s)",
                                  covariate, values[1], values[2]),
                      y = NULL, title = paste("Effect of", covariate)) +
        ggplot2::theme_minimal(base_size = 12)
    } else {  # pointestimate
      preds$level <- factor(preds$value, levels = grid)
      ggplot2::ggplot(preds, ggplot2::aes(.data$est, .data$level)) +
        ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
                               orientation = "y", width = 0.2) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::facet_wrap(~ topic, scales = "free_x") +
        ggplot2::labs(x = "expected topic proportion", y = covariate,
                      title = paste("Effect of", covariate)) +
        ggplot2::theme_minimal(base_size = 12)
    }
  }
}

#' Topic correlation network
#'
#' Nodes are topics (sized by prevalence, labelled by top words); edges join
#' topics whose proportions are positively correlated above `cutoff`. Uses a
#' lightweight circular layout — no graph-library dependency.
#'
#' @param model A faSTM fit.
#' @param cutoff Correlation threshold for an edge.
#' @param n Top words per topic label.
#' @param labeltype Word ranking for labels.
#' @return A ggplot object.
#' @export
plot_topic_network <- function(model, cutoff = 0.03, n = 3L, labeltype = "frex") {
  .need_ggplot()
  tc <- topic_correlation(model, cutoff = cutoff)
  K <- ncol(tc$cor)
  ang <- seq(0, 2 * pi, length.out = K + 1)[-(K + 1)]
  nodes <- data.frame(topic = seq_len(K), x = cos(ang), y = sin(ang),
                      prev = colMeans(model$theta),
                      label = apply(label_topics(model, n = n)[[labeltype]], 1L, paste, collapse = ", "))
  ut <- which(upper.tri(tc$posadj) & tc$posadj == 1L, arr.ind = TRUE)
  edges <- if (nrow(ut) > 0)
    data.frame(x = nodes$x[ut[, 1]], y = nodes$y[ut[, 1]],
               xend = nodes$x[ut[, 2]], yend = nodes$y[ut[, 2]],
               w = tc$cor[ut]) else
    data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), w = numeric(0))

  g <- ggplot2::ggplot()
  if (nrow(edges) > 0)
    g <- g + ggplot2::geom_segment(data = edges,
              ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend, linewidth = .data$w),
              color = "#9ecae1", alpha = 0.8) +
            ggplot2::scale_linewidth(range = c(0.3, 2.5), guide = "none")
  g <- g + ggplot2::geom_point(data = nodes, ggplot2::aes(.data$x, .data$y, size = .data$prev),
                          color = "#2c7fb8") +
    ggplot2::geom_text(data = nodes, ggplot2::aes(.data$x, .data$y, label = .data$topic),
                       color = "white", size = 3)
  ## ggrepel keeps word labels off the nodes/edges; degrade to a fixed offset.
  g <- if (requireNamespace("ggrepel", quietly = TRUE))
    g + ggrepel::geom_text_repel(data = nodes,
          ggplot2::aes(.data$x, .data$y, label = .data$label),
          size = 2.8, color = "grey25", point.padding = 0.6, box.padding = 0.5,
          min.segment.length = 0.4, seed = 1)
  else
    g + ggplot2::geom_text(data = nodes,
          ggplot2::aes(.data$x, .data$y, label = paste0("\n\n", .data$label)),
          size = 2.8, color = "grey25")
  g +
    ggplot2::scale_size(range = c(4, 12), guide = "none") +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(title = "Topic correlation network") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 20, 20))
}

## ---- effect prediction helpers --------------------------------------------

# Build a representative newdata: vary `covariate` over `vals`, hold the other
# model variables at their mean (numeric) or modal level (factor).
.effect_newdata <- function(x, covariate, vals) {
  meta <- x$metadata
  vars <- all.vars(x$formula[-2L])               # RHS variables
  template <- lapply(setdiff(vars, covariate), function(v) {
    col <- meta[[v]]
    if (is.numeric(col)) mean(col, na.rm = TRUE)
    else { f <- as.factor(col); factor(names(which.max(table(f))), levels = levels(f)) }
  })
  names(template) <- setdiff(vars, covariate)
  base <- if (length(template)) as.data.frame(template, stringsAsFactors = FALSE)[rep(1, length(vals)), , drop = FALSE]
          else data.frame(row.names = seq_along(vals))
  cv <- meta[[covariate]]
  base[[covariate]] <- if (is.numeric(cv)) as.numeric(vals) else factor(vals, levels = levels(as.factor(cv)))
  base
}

# Design matrix for newdata, reusing the fit's terms (spline knots / factor levels).
.effect_design <- function(x, newdata) {
  mf <- stats::model.frame(x$mterms, newdata, xlev = x$xlevels)
  stats::model.matrix(x$mterms, mf)
}

# Predicted proportion + se for each (topic, value).
.effect_grid <- function(x, covariate, vals, topics) {
  Xn <- .effect_design(x, .effect_newdata(x, covariate, vals))
  do.call(rbind, lapply(topics, function(k) {
    co <- x$coefficients[[paste0("topic", k)]]
    est <- as.numeric(Xn %*% co$est)
    se  <- sqrt(pmax(0, rowSums((Xn %*% co$vcov) * Xn)))
    data.frame(topic = k, value = vals, est = est, se = se)
  }))
}

# Difference between two covariate values: contrast c = x(v1) - x(v0).
.effect_difference <- function(x, covariate, values, topics) {
  Xn <- .effect_design(x, .effect_newdata(x, covariate, values))
  cvec <- Xn[1, ] - Xn[2, ]
  do.call(rbind, lapply(topics, function(k) {
    co <- x$coefficients[[paste0("topic", k)]]
    data.frame(topic = k, est = sum(cvec * co$est),
               se = sqrt(as.numeric(t(cvec) %*% co$vcov %*% cvec)))
  }))
}
