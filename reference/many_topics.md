# Select models across a range of K

Runs
[`select_model()`](https://nealcaren.github.io/faSTM/reference/select_model.md)
for each K and returns the chosen model per K.

## Usage

``` r
many_topics(
  corpus,
  K,
  N = 10L,
  prevalence = NULL,
  content = NULL,
  by = "sum",
  cores = 1L,
  seed = 1L,
  ...
)
```

## Arguments

- corpus:

  A `faSTM_corpus`.

- K:

  Integer vector of topic counts.

- N:

  Number of candidate models (distinct random inits).

- prevalence, content:

  Optional covariate formulas.

- by:

  Selection rule passed to
  [`select_best()`](https://nealcaren.github.io/faSTM/reference/select_best.md).

- cores:

  Candidates to fit in parallel.

- seed:

  Base RNG seed (candidate i uses `seed + i - 1`).

- ...:

  Passed to
  [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md).

## Value

A `faSTM_manytopics`: `models` (best per K) and a `summary` data.frame.
