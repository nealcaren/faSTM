# Top terms per topic, with their numeric scores (tidy)

Like
[`label_topics()`](https://nealcaren.github.io/faSTM/reference/label_topics.md)
but returns the *values* behind the ranking, not just the words — e.g.
the numeric FREX score per top term (stm issue \#265).

## Usage

``` r
topic_terms(
  model,
  n = 7L,
  by = c("prob", "frex", "lift", "score"),
  frexweight = 0.5
)
```

## Arguments

- model:

  A faSTM fit.

- n:

  Terms per topic.

- by:

  Ranking measure: `"prob"`, `"frex"`, `"lift"`, or `"score"`.

- frexweight:

  FREX frequency/exclusivity weight (used when `by = "frex"`).

## Value

A tidy data.frame with `topic`, `rank`, `term`, `score`, `measure`.
