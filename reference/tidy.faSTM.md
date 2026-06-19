# Tidy a faSTM fit (topic-term or document-topic distributions)

Tidy a faSTM fit (topic-term or document-topic distributions)

## Usage

``` r
# S3 method for class 'faSTM'
tidy(x, matrix = c("beta", "gamma", "frex"), ...)
```

## Arguments

- x:

  A faSTM fit.

- matrix:

  `"beta"` (topic-term probabilities), `"gamma"` (document-topic
  proportions), or `"frex"` (topic-term FREX scores).

- ...:

  Unused.

## Value

A tidy data.frame.
