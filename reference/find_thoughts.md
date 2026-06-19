# Representative documents for each topic

Representative documents for each topic

## Usage

``` r
find_thoughts(model, texts = NULL, topics = NULL, n = 3L)
```

## Arguments

- model:

  A faSTM fit.

- texts:

  Optional character vector of the raw document texts, aligned to the
  fitted documents; returned alongside the indices when supplied.

- topics:

  Topics to report (default all).

- n:

  Documents per topic.

## Value

A list with `index` (per-topic document indices) and, if `texts` is
given, `docs` (the texts).
