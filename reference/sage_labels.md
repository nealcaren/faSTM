# Labels for a content (SAGE) model

For models fit with a `content` covariate, reports each topic's marginal
top words plus, for every content group, the words most distinctive to
that group within the topic (group-vs-marginal log-ratio — the SAGE
deviation).

## Usage

``` r
sage_labels(model, n = 7L, frexweight = NULL)
```

## Arguments

- model:

  A faSTM fit with a content covariate.

- n:

  Words per list.

## Value

A `faSTM_sagelabels` object.
