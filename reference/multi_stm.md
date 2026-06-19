# Cross-run topic stability

Aligns every model from a
[`select_model()`](https://nealcaren.github.io/faSTM/reference/select_model.md)
run to the first and reports how stable each topic's top words are
across runs (cf.
[`stm::multiSTM`](https://rdrr.io/pkg/stm/man/multiSTM.html)).

## Usage

``` r
multi_stm(x, n = 10L)
```

## Arguments

- x:

  A `faSTM_selectmodel`.

- n:

  Top words used for the stability score.

## Value

A `faSTM_multistm` with a per-topic mean top-word agreement.
