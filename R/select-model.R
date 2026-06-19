#' Fit several models and keep the ones on the quality frontier
#'
#' With random initialization the variational objective is multimodal, so the
#' standard workflow (cf. `stm::selectModel`) is to fit many models and keep
#' those on the semantic-coherence / exclusivity frontier, then choose among
#' them. faSTM fits the candidates in parallel.
#'
#' @param corpus A `faSTM_corpus`.
#' @param K Number of topics.
#' @param N Number of candidate models (distinct random inits).
#' @param prevalence,content Optional covariate formulas.
#' @param init.type Initialization; `"Random"` (the point of selecting) or
#'   `"Spectral"` (deterministic — then all `N` are identical).
#' @param cores Candidates to fit in parallel.
#' @param M Top words for coherence/exclusivity scoring.
#' @param frexw Exclusivity FREX weight.
#' @param seed Base RNG seed (candidate i uses `seed + i - 1`).
#' @param ... Passed to [stm()].
#'
#' @return A `faSTM_selectmodel`: `models` (the fits), `semcoh`, `exclusivity`,
#'   and `frontier` (indices of non-dominated models).
#' @export
select_model <- function(corpus, K, N = 10L, prevalence = NULL, content = NULL,
                         init.type = "Random", cores = 1L, M = 10L, frexw = 0.7,
                         seed = 1L, ...) {
  stopifnot(inherits(corpus, "faSTM_corpus"))
  seeds <- seed + seq_len(N) - 1L
  threads <- if (cores > 1L) max(1L, parallel::detectCores() %/% cores) else 0L
  has_content <- !is.null(content)

  fit_one <- function(s) {
    fit <- stm(corpus, K = K, prevalence = prevalence, content = content,
               init.type = init.type, seed = s, num_threads = threads,
               verbose = FALSE, ...)
    list(fit = fit,
         semcoh = mean(semantic_coherence(fit, M = M)),
         excl = if (has_content) NA_real_ else mean(exclusivity(fit, M = M, frexw = frexw)))
  }
  runs <- if (cores > 1L && .Platform$OS.type != "windows")
    parallel::mclapply(seeds, fit_one, mc.cores = cores, mc.set.seed = FALSE)
  else lapply(seeds, fit_one)

  semcoh <- vapply(runs, `[[`, numeric(1), "semcoh")
  excl   <- vapply(runs, `[[`, numeric(1), "excl")
  frontier <- .pareto_frontier(semcoh, excl)

  structure(list(models = lapply(runs, `[[`, "fit"),
                 semcoh = semcoh, exclusivity = excl,
                 frontier = frontier, seeds = seeds, M = M, frexw = frexw),
            class = "faSTM_selectmodel")
}

# Non-dominated set maximizing both coherence and exclusivity. With content
# (exclusivity = NA) the frontier is by coherence alone.
.pareto_frontier <- function(semcoh, excl) {
  n <- length(semcoh)
  if (all(is.na(excl))) return(which(semcoh == max(semcoh)))
  dominated <- logical(n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) next
    if (semcoh[j] >= semcoh[i] && excl[j] >= excl[i] &&
        (semcoh[j] > semcoh[i] || excl[j] > excl[i])) { dominated[i] <- TRUE; break }
  }
  which(!dominated)
}

#' Pick one model from a `select_model` run
#'
#' @param x A `faSTM_selectmodel`.
#' @param by `"semcoh"`, `"exclusivity"`, or `"sum"` (rank-sum of both).
#' @return A single faSTM fit.
#' @export
select_best <- function(x, by = c("sum", "semcoh", "exclusivity")) {
  by <- match.arg(by)
  f <- x$frontier
  score <- switch(by,
    semcoh = x$semcoh[f],
    exclusivity = x$exclusivity[f],
    sum = rank(x$semcoh[f]) + rank(ifelse(is.na(x$exclusivity[f]), 0, x$exclusivity[f])))
  x$models[[f[which.max(score)]]]
}

#' @export
print.faSTM_selectmodel <- function(x, ...) {
  cat(sprintf("faSTM select_model — %d candidates (frontier: %s)\n",
              length(x$models), paste(x$frontier, collapse = ", ")))
  print(data.frame(model = seq_along(x$models),
                   semcoh = round(x$semcoh, 2),
                   exclusivity = round(x$exclusivity, 2),
                   frontier = seq_along(x$models) %in% x$frontier),
        row.names = FALSE)
  invisible(x)
}

#' Select models across a range of K
#'
#' Runs [select_model()] for each K and returns the chosen model per K.
#'
#' @inheritParams select_model
#' @param K Integer vector of topic counts.
#' @param by Selection rule passed to [select_best()].
#' @return A `faSTM_manytopics`: `models` (best per K) and a `summary` data.frame.
#' @export
many_topics <- function(corpus, K, N = 10L, prevalence = NULL, content = NULL,
                        by = "sum", cores = 1L, seed = 1L, ...) {
  K <- as.integer(K)
  per_k <- lapply(K, function(k) {
    sel <- select_model(corpus, K = k, N = N, prevalence = prevalence,
                        content = content, cores = cores, seed = seed, ...)
    list(model = select_best(sel, by = by),
         semcoh = mean(sel$semcoh[sel$frontier]),
         excl = mean(sel$exclusivity[sel$frontier]))
  })
  structure(list(
    models = lapply(per_k, `[[`, "model"),
    summary = data.frame(K = K,
                         semcoh = vapply(per_k, `[[`, numeric(1), "semcoh"),
                         exclusivity = vapply(per_k, `[[`, numeric(1), "excl"))),
    class = "faSTM_manytopics")
}

#' @export
print.faSTM_manytopics <- function(x, ...) {
  cat("faSTM many_topics — best model per K\n")
  print(x$summary, row.names = FALSE); invisible(x)
}
