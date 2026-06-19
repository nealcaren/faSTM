# Plot estimated covariate effects on topic prevalence

Plot estimated covariate effects on topic prevalence

## Usage

``` r
# S3 method for class 'faSTM_effect'
plot(
  x,
  covariate,
  method = c("pointestimate", "continuous", "difference"),
  topics = x$topics,
  model = NULL,
  cov.value1 = NULL,
  cov.value2 = NULL,
  values = NULL,
  moderator = NULL,
  moderator.value = NULL,
  npoints = 50L,
  ci = 0.95,
  labeltype = NULL,
  custom.labels = NULL,
  xlab = NULL,
  main = NULL,
  ...
)
```

## Arguments

- x:

  A `faSTM_effect` (from
  [`estimateEffect()`](https://nealcaren.github.io/faSTM/reference/estimateEffect.md)).

- covariate:

  Name of the covariate to vary.

- method:

  `"pointestimate"` (mean proportion per level of a categorical
  covariate), `"continuous"` (proportion vs a numeric covariate, with
  ribbon), or `"difference"` (difference between two `values`).

- topics:

  Topics to show (default all in the effect object).

- values:

  For `"difference"`, length-2 `c(high, low)`; for `"continuous"`,
  optional range; ignored for `"pointestimate"`.

- npoints:

  Grid size for `"continuous"`.

- ci:

  Confidence level.

- ...:

  Unused.

## Value

A ggplot object.
