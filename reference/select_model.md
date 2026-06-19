# Fit several models and keep the ones on the quality frontier

With random initialization the variational objective is multimodal, so
the standard workflow (cf.
[`stm::selectModel`](https://rdrr.io/pkg/stm/man/selectModel.html)) is
to fit many models and keep those on the semantic-coherence /
exclusivity frontier, then choose among them. faSTM fits the candidates
in parallel.

## Usage

``` r
select_model(
  corpus,
  K,
  N = 10L,
  prevalence = NULL,
  content = NULL,
  init.type = "Random",
  cores = 1L,
  M = 10L,
  frexw = 0.7,
  seed = 1L,
  ...
)
```

## Arguments

- corpus:

  A `faSTM_corpus`.

- K:

  Number of topics.

- N:

  Number of candidate models (distinct random inits).

- prevalence, content:

  Optional covariate formulas.

- init.type:

  Initialization; `"Random"` (the point of selecting) or `"Spectral"`
  (deterministic — then all `N` are identical).

- cores:

  Candidates to fit in parallel.

- M:

  Top words for coherence/exclusivity scoring.

- frexw:

  Exclusivity FREX weight.

- seed:

  Base RNG seed (candidate i uses `seed + i - 1`).

- ...:

  Passed to
  [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md).

## Value

A `faSTM_selectmodel`: `models` (the fits), `semcoh`, `exclusivity`, and
`frontier` (indices of non-dominated models).
