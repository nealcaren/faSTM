# stm-compatible label scorers (FREX / lift / score)

Ports of `stm:::calcfrex`/`calclift`/`calcscore`. Each takes a K x V
`logbeta` (log topic-word matrix) and returns a V x K matrix whose
columns are the word indices ordered most- to least-characteristic for
each topic.

## Usage

``` r
calcfrex(logbeta, w = 0.5, wordcounts = NULL)

calclift(logbeta, wordcounts)

calcscore(logbeta)
```

## Arguments

- logbeta:

  K x V log topic-word matrix.

- w:

  FREX frequency/exclusivity weight.

- wordcounts:

  Corpus term frequencies (enables the James-Stein shrinkage).

## Value

A V x K matrix of ordered word indices.
