# Document-topic proportions as a data frame

Document-topic proportions as a data frame

## Usage

``` r
make_dt(model, meta = NULL)
```

## Arguments

- model:

  A faSTM fit.

- meta:

  Optional metadata to bind alongside (defaults to none).

## Value

A data.frame with `document` and `Topic1..TopicK` columns (+ `meta`).
