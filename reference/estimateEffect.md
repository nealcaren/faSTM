# Estimate covariate effects on topic prevalence (method of composition)

A drop-in for
[`stm::estimateEffect()`](https://rdrr.io/pkg/stm/man/estimateEffect.html)
that propagates per-document topic-estimation uncertainty honestly: it
regresses each posterior draw of topic proportions on the covariates and
pools the per-draw fits by Rubin's rules. This is topica's "honest"
effect estimator, the reason faSTM ships its own rather than inheriting
stm's.

## Usage

``` r
estimateEffect(
  formula,
  stmobj,
  metadata = meta,
  uncertainty = c("Global", "None", "Local"),
  nsims = 100L,
  seed = NULL,
  meta = NULL,
  documents = NULL,
  combine = NULL,
  weights = NULL,
  cluster = NULL,
  ...
)
```

## Arguments

- formula:

  A formula whose LHS lists topic numbers (e.g. `1:5 ~ treatment`) or
  whose LHS is empty to use all topics; RHS gives the covariates.
  Random- effect terms `(term | group)` are supported (fits
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) per draw and
  pools the fixed effects; variance components are stored).

- stmobj:

  A faSTM fit (from
  [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md)).

- metadata:

  A data.frame of covariates aligned to the documents.

- uncertainty:

  `"Global"` (method of composition over posterior draws, default) or
  `"None"` (single OLS on the posterior-mean theta).

- nsims:

  Posterior draws for `uncertainty = "Global"`.

- seed:

  Optional seed for the posterior draws.

- documents:

  Accepted for stm compatibility (faSTM reads nu from the fit).

- combine:

  Optional list of topic vectors to also estimate as aggregate topics
  (each set's proportions are summed before regressing); named entries
  set the coefficient names. E.g. `combine = list(econ = c(3, 7))`.

- weights:

  Optional per-document survey/sampling weights (weighted OLS).

- cluster:

  Optional per-document cluster ids for cluster-robust SEs.

- ...:

  Unused (stm signature compatibility).

## Value

An object of class `c("faSTM_effect", "estimateEffect")` with a
[`summary()`](https://rdrr.io/r/base/summary.html) method, holding
pooled coefficients and standard errors per topic.
