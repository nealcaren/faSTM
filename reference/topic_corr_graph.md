# Topic-correlation network as an igraph graph

Exports the positive-correlation topic network as an `igraph` object
(stm issue \#242), with topic prevalence and FREX labels as vertex
attributes and the positive correlations as edge weights — ready for
igraph/ggraph layouts.

## Usage

``` r
topic_corr_graph(x, model = NULL, nlabel = 3L)
```

## Arguments

- x:

  A `faSTM_topiccorr` (from
  [`topicCorr()`](https://rdrr.io/pkg/stm/man/topicCorr.html)) or a
  faSTM fit.

- model:

  The fit, if `x` is a bare correlation object (for vertex
  prevalence/labels).

- nlabel:

  Top FREX words per topic for the vertex label.

## Value

An undirected `igraph` graph.
