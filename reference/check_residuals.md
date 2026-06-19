# Residual dispersion check (is K large enough?)

Multinomial residual dispersion (Taddy 2012; port of
[`stm::checkResiduals`](https://rdrr.io/pkg/stm/man/checkResiduals.html)).
A dispersion well above 1 suggests too few topics.

## Usage

``` r
check_residuals(model, tol = 0.01)
```

## Arguments

- model:

  A faSTM fit (carries its documents).

- tol:

  Threshold for counting estimable residual cells.

## Value

A list with `dispersion`, `pvalue`, and `df`.
