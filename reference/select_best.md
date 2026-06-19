# Pick one model from a `select_model` run

Pick one model from a `select_model` run

## Usage

``` r
select_best(x, by = c("sum", "semcoh", "exclusivity"))
```

## Arguments

- x:

  A `faSTM_selectmodel`.

- by:

  `"semcoh"`, `"exclusivity"`, or `"sum"` (rank-sum of both).

## Value

A single faSTM fit.
