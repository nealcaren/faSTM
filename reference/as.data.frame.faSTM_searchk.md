# Convert search_k diagnostics to long form for plotting

Returns a long data.frame (`K`, `metric`, `value`) ready for ggplot2 —
`ggplot(as.data.frame(res), aes(K, value)) + geom_line() + facet_wrap(~metric, scales = "free_y")`.

## Usage

``` r
# S3 method for class 'faSTM_searchk'
as.data.frame(x, ...)
```

## Arguments

- x:

  A `faSTM_searchk`.

- ...:

  Unused.
