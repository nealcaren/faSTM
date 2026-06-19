# Expected topic proportions (the numbers behind the summary plot)

Returns the corpus-level expected topic proportions — the mean of theta
per topic — as a numeric table, so you can read off the values stm's
`plot(type = "summary")` displays (stm issue \#269).

## Usage

``` r
topic_proportions(model, nlabel = 3L)
```

## Arguments

- model:

  A faSTM fit.

- nlabel:

  Top FREX words to attach as a topic label.

## Value

A data.frame with `topic`, `proportion`, `label`, sorted by proportion.
