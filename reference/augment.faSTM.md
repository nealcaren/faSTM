# Augment: most-likely topic for each document-term token

Assigns each (document, term) cell to the topic maximizing
`theta[doc, k] * beta[k, term]` (cf.
[`tidytext::augment.STM`](https://juliasilge.github.io/tidytext/reference/stm_tidiers.html)).

## Usage

``` r
# S3 method for class 'faSTM'
augment(x, data = NULL, ...)
```

## Arguments

- x:

  A faSTM fit (carries its DTM).

- data:

  Ignored (accepted for the generic).

- ...:

  Unused.

## Value

A data.frame: `document`, `term`, `count`, `.topic`.
