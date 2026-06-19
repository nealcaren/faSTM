# Plot a fitted model

Plot a fitted model

## Usage

``` r
# S3 method for class 'faSTM'
plot(
  x,
  type = c("summary", "labels", "perspectives", "hist"),
  topics = NULL,
  n = 5L,
  labeltype = "frex",
  ...
)
```

## Arguments

- x:

  A faSTM fit.

- type:

  `"summary"` (topics ranked by expected prevalence + top words),
  `"labels"` (top words per topic), `"perspectives"` (word comparison
  between two topics, or between content-covariate groups of one topic),
  or `"hist"` (distribution of document-topic proportions).

- topics:

  Topics to show (for `"perspectives"`: one topic in a content model, or
  two topics to compare).

- n:

  Top words to label each topic with.

- labeltype:

  Word ranking for labels: `"frex"`, `"prob"`, `"lift"`, `"score"`.

- ...:

  Accepted for stm compatibility (e.g. `xlim`); mostly ignored.

## Value

A ggplot object.
