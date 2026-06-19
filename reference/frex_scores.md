# FREX scores for every word and topic

FREX balances word *frequency* and *exclusivity* (Bischof & Airoldi
2012; Roberts et al.). Unlike `stm`'s `labelTopics()`, this returns the
full numeric FREX matrix, not just the ranked words (addresses a
long-standing `stm` request, bstewart/stm#265).

## Usage

``` r
frex_scores(model, w = 0.5)
```

## Arguments

- model:

  A faSTM fit.

- w:

  FREX frequency/exclusivity weight (0.5 = equal).

## Value

A topics × vocabulary matrix of FREX scores (columns named by vocab).
