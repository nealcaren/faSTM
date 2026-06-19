#' Search over the number of topics K
#'
#' Fits the model across a range of K and reports diagnostics for choosing it:
#' held-out likelihood (document completion), semantic coherence, exclusivity,
#' and the variational bound. Unlike `stm::searchK`, the per-K fits parallelize
#' across K (a long-standing request, bstewart/stm#262) and each fit is itself
#' fast (Rust), so a sweep that took minutes takes seconds.
#'
#' @param corpus A `faSTM_corpus` (from [as_corpus()]).
#' @param K Integer vector of topic counts to try.
#' @param prevalence,content Optional covariate formulas (see [stm()]).
#' @param heldout Logical; compute held-out likelihood via document completion.
#' @param proportion Held-out token fraction (passed to [make_heldout()]).
#' @param cores Number of K-fits to run in parallel (forked; 1 = sequential).
#'   When `cores > 1` each fit runs single-threaded to avoid oversubscription;
#'   when `cores == 1` each fit uses all cores.
#' @param M Top words for coherence/exclusivity.
#' @param seed RNG seed (held-out split + fits).
#' @param ... Passed to [stm()] (e.g. `max.em.its`, `init.type`).
#'
#' @return A `faSTM_searchk` object wrapping a tidy data.frame `results` with one
#'   row per K (`K`, `heldout`, `semcoh`, `exclusivity`, `bound`).
#' @export
search_k <- function(corpus, K, prevalence = NULL, content = NULL,
                     heldout = TRUE, proportion = 0.5, residuals = FALSE,
                     cores = 1L, M = 10L, seed = 1L,
                     measure = c("mimno", "npmi", "c_v"), ...) {
  stopifnot(inherits(corpus, "faSTM_corpus"))
  measure <- match.arg(measure)
  K <- as.integer(K)

  ho <- if (heldout) make_heldout(corpus, proportion = proportion, seed = seed) else NULL
  fit_corpus <- if (heldout) ho$corpus else corpus
  ## Each fit's E-step is already multithreaded, so split the machine across the
  ## K-workers rather than oversubscribing: cores workers x (cores_total/cores)
  ## threads keeps all cores busy. cores=1 lets a single fit use them all.
  threads <- if (cores > 1L) max(1L, parallel::detectCores() %/% cores) else 0L
  has_content <- !is.null(content)

  one_k <- function(k) {
    fit <- stm(fit_corpus, K = k, prevalence = prevalence, content = content,
               seed = seed, num_threads = threads, verbose = FALSE, ...)
    data.frame(
      K           = k,
      heldout     = if (heldout) eval_heldout(fit, ho) else NA_real_,
      semcoh      = mean(coherence(fit, measure = measure, M = M)),
      exclusivity = if (has_content) NA_real_ else mean(exclusivity(fit, M = M)),
      residual    = if (residuals) check_residuals(fit)$dispersion else NA_real_,
      bound       = tail(fit$convergence$bound, 1L)
    )
  }

  rows <- if (cores > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(K, one_k, mc.cores = cores, mc.set.seed = FALSE)
  } else {
    lapply(K, one_k)
  }
  results <- do.call(rbind, rows)
  results <- results[order(results$K), , drop = FALSE]
  rownames(results) <- NULL
  structure(list(results = results, heldout = heldout), class = "faSTM_searchk")
}

#' @export
print.faSTM_searchk <- function(x, ...) {
  cat("faSTM search_k — diagnostics by number of topics\n")
  r <- x$results
  show <- data.frame(K = r$K,
                     heldout = round(r$heldout, 3),
                     semcoh = round(r$semcoh, 1),
                     exclus = round(r$exclusivity, 2),
                     bound = round(r$bound, 0))
  print(show, row.names = FALSE)
  cat("\nHigher held-out likelihood and semantic coherence are better; ",
      "coherence and exclusivity trade off.\n", sep = "")
  invisible(x)
}

#' Convert search_k diagnostics to long form for plotting
#'
#' Returns a long data.frame (`K`, `metric`, `value`) ready for ggplot2 —
#' `ggplot(as.data.frame(res), aes(K, value)) + geom_line() + facet_wrap(~metric, scales = "free_y")`.
#' @param x A `faSTM_searchk`.
#' @param ... Unused.
#' @export
as.data.frame.faSTM_searchk <- function(x, ...) {
  r <- x$results
  metrics <- c("heldout", "semcoh", "exclusivity", "residual", "bound")
  metrics <- metrics[vapply(metrics, function(m) any(is.finite(r[[m]])), logical(1))]
  do.call(rbind, lapply(metrics, function(m)
    data.frame(K = r$K, metric = m, value = r[[m]])))
}
