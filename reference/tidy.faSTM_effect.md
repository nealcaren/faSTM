# Tidy an estimateEffect fit (one row per term per topic)

Tidy an estimateEffect fit (one row per term per topic)

## Usage

``` r
# S3 method for class 'faSTM_effect'
tidy(x, ...)
```

## Arguments

- x:

  A `faSTM_effect`.

- ...:

  Unused.

## Value

A data.frame: `topic`, `term`, `estimate`, `std.error`, `statistic`,
`p.value`.
