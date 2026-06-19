# Flag words that load almost entirely on one topic

Port of `stm:::checkBeta`: finds (topic, word) cells whose
`exp(logbeta)` exceeds `1 - tolerance` — words that are nearly exclusive
to a single topic, which can destabilize estimation.

## Usage

``` r
checkBeta(stmobject, tolerance = 0.01)
```

## Arguments

- stmobject:

  A faSTM/stm fit.

- tolerance:

  Threshold; a word with topic-probability `> 1 - tolerance` is flagged.

## Value

A list with `problemTopics`, `problemWords`, and error counts per
content group.
