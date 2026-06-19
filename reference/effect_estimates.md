# Extract estimateEffect estimates as a tidy data.frame (no plotting)

Returns the point estimates, standard errors and confidence bounds that
[`plot.faSTM_effect()`](https://nealcaren.github.io/faSTM/reference/plot.faSTM_effect.md)
would draw, so you can build a custom plot or table (stm issue \#83).
Same arguments as the plot method.

## Usage

``` r
effect_estimates(
  x,
  covariate,
  method = c("pointestimate", "continuous", "difference"),
  topics = x$topics,
  cov.value1 = NULL,
  cov.value2 = NULL,
  values = NULL,
  moderator = NULL,
  moderator.value = NULL,
  npoints = 50L,
  ci = 0.95
)
```

## Arguments

- x:

  A `faSTM_effect` (from
  [`estimateEffect()`](https://nealcaren.github.io/faSTM/reference/estimateEffect.md)).

- covariate:

  Covariate name.

- method:

  `"pointestimate"`, `"continuous"`, or `"difference"`.

- topics:

  Topics to include.

- cov.value1, cov.value2, values:

  Levels/values for difference or continuous range.

- moderator, moderator.value:

  Optional held-fixed interaction term.

- npoints:

  Grid size for `"continuous"`.

- ci:

  Confidence level for `lower`/`upper`.

## Value

A data.frame with `topic`, `value`, `est`, `se`, `lower`, `upper`.
