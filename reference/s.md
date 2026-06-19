# Spline term for prevalence formulas

A b-spline basis for smooth covariate effects, e.g.
`prevalence = ~ s(day)`. Matches
[`stm::s()`](https://rdrr.io/pkg/stm/man/s.html) exactly — including the
`df = min(10, nval - 1)` default — so spline-term coefficients agree
with `stm`. (You can also use
[`splines::bs()`](https://rdrr.io/r/splines/bs.html)/[`splines::ns()`](https://rdrr.io/r/splines/ns.html)
directly.)

## Usage

``` r
s(x, df, ...)
```

## Arguments

- x:

  Numeric predictor.

- df:

  Basis dimension; defaults to `min(10, length(unique(x)) - 1)`.

- ...:

  Passed to [`splines::bs()`](https://rdrr.io/r/splines/bs.html).

## Value

A spline basis matrix (with class `"s"`).
