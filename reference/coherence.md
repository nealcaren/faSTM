# Topic coherence (Mimno / NPMI / c_v)

Coherence scores for each topic's top-`M` words, computed from the fit's
stored document-term matrix. `"mimno"` is the UMass-style score of
[`semantic_coherence()`](https://nealcaren.github.io/faSTM/reference/semantic_coherence.md);
`"npmi"` averages pairwise normalized PMI; `"c_v"` is the Roeder et al.
(2015) measure (one-set segmentation, NPMI confirmation, cosine
aggregation). NPMI/c_v use *document* co-occurrence as the probability
estimator. Higher is more coherent (npmi/c_v are roughly in -1, 1).

## Usage

``` r
coherence(model, measure = c("mimno", "npmi", "c_v"), M = 10L)
```

## Arguments

- model:

  A faSTM fit (carries its DTM).

- measure:

  `"mimno"`, `"npmi"`, or `"c_v"`.

- M:

  Top words per topic.

## Value

A numeric vector, one coherence score per topic.
