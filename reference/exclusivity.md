# Topic exclusivity (FREX-summary, frexw default 0.7)

Topic exclusivity (FREX-summary, frexw default 0.7)

## Usage

``` r
exclusivity(model, M = 10L, frexw = 0.7)
```

## Arguments

- model:

  A faSTM fit.

- M:

  Top words per topic.

- frexw:

  Frequency/exclusivity weight.

## Value

A numeric vector, one exclusivity value per topic.
