# Permutation test for a binary covariate's effect on topics

Refits the model many times with the treatment labels permuted, aligning
topics across refits, to build a null distribution for the treatment
effect on each topic (cf.
[`stm::permutationTest`](https://rdrr.io/pkg/stm/man/permutationTest.html)).
Fast because each refit is cheap.

## Usage

``` r
permutation_test(
  formula,
  model,
  treatment,
  corpus,
  nruns = 100L,
  seed = NULL,
  ...
)
```

## Arguments

- formula:

  Prevalence formula whose RHS includes `treatment`.

- model:

  A faSTM fit.

- treatment:

  Name of a 0/1 covariate in `corpus$meta`.

- corpus:

  The `faSTM_corpus` the model was fit on.

- nruns:

  Total models (1 reference + `nruns-1` permutations).

- seed:

  RNG seed.

- ...:

  Passed to
  [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md) for the
  refits.

## Value

A `faSTM_permtest` with `ref` (observed per-topic effects) and `null`
(`(nruns-1)` × K permuted effects).
