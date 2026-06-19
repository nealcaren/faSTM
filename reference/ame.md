# Average marginal effects from an estimateEffect fit

The average expected change in a topic's proportion per unit of a
covariate (continuous: average derivative; factor: average
level-vs-reference contrast), averaged over the observed data. Cleaner
than reading raw coefficients, especially with splines/interactions (cf.
the `margins` package; stm \#271).

## Usage

``` r
ame(object, covariate, topics = object$topics, h = NULL, ci = 0.95)
```

## Arguments

- object:

  A `faSTM_effect` (from
  [`estimateEffect()`](https://nealcaren.github.io/faSTM/reference/estimateEffect.md)).

- covariate:

  Covariate name.

- topics:

  Topics to report (default: all in the fit).

- h:

  Step for the numeric derivative (continuous covariates); defaults to
  `0.01 * sd`.

- ci:

  Confidence level.

## Value

A data.frame: `topic`, `term`, `ame`, `se`, `lower`, `upper`.
