# Label topics by top words (prob, FREX, lift, score)

Label topics by top words (prob, FREX, lift, score)

## Usage

``` r
label_topics(model, n = 7L, frexweight = 0.5)
```

## Arguments

- model:

  A faSTM fit.

- n:

  Number of words per topic per metric.

- frexweight:

  FREX frequency/exclusivity weight.

## Value

A `faSTM_labels` object: per-metric top-word matrices (`prob`, `frex`,
`lift`, `score`), each topics × `n`.
