# Search over the number of topics K

Fits the model across a range of K and reports diagnostics for choosing
it: held-out likelihood (document completion), semantic coherence,
exclusivity, and the variational bound. Unlike
[`stm::searchK`](https://rdrr.io/pkg/stm/man/searchK.html), the per-K
fits parallelize across K (a long-standing request, bstewart/stm#262)
and each fit is itself fast (Rust), so a sweep that took minutes takes
seconds.

## Usage

``` r
search_k(
  corpus,
  K,
  prevalence = NULL,
  content = NULL,
  heldout = TRUE,
  proportion = 0.5,
  residuals = FALSE,
  cores = 1L,
  M = 10L,
  seed = 1L,
  measure = c("mimno", "npmi", "c_v"),
  verbose = FALSE,
  ...
)
```

## Arguments

- corpus:

  A `faSTM_corpus` (from
  [`as_corpus()`](https://nealcaren.github.io/faSTM/reference/as_corpus.md)).

- K:

  Integer vector of topic counts to try.

- prevalence, content:

  Optional covariate formulas (see
  [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md)).

- heldout:

  Logical; compute held-out likelihood via document completion.

- proportion:

  Held-out token fraction (passed to
  [`make_heldout()`](https://nealcaren.github.io/faSTM/reference/make_heldout.md)).

- cores:

  Number of K-fits to run in parallel (forked; 1 = sequential). When
  `cores > 1` each fit runs single-threaded to avoid oversubscription;
  when `cores == 1` each fit uses all cores.

- M:

  Top words for coherence/exclusivity.

- seed:

  RNG seed (held-out split + fits).

- ...:

  Passed to
  [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md) (e.g.
  `max.em.its`, `init.type`).

## Value

A `faSTM_searchk` object wrapping a tidy data.frame `results` with one
row per K (`K`, `heldout`, `semcoh`, `exclusivity`, `bound`).
