# Semantic coherence (Mimno et al. 2011)

Sum over the top-`M` words of each topic of
`log((D(w_i,w_j)+1)/D(w_j))`, using document co-occurrence counts.
Higher (less negative) is more coherent.

## Usage

``` r
semantic_coherence(model, M = 10L)
```

## Arguments

- model:

  A faSTM fit (must carry its document-term matrix; faSTM stores it).

- M:

  Number of top words per topic.

## Value

A numeric vector, one coherence value per topic.
