# Topic correlation graph (positive correlations of topic proportions)

Topic correlation graph (positive correlations of topic proportions)

## Usage

``` r
topic_correlation(model, cutoff = 0.01)
```

## Arguments

- model:

  A faSTM fit.

- cutoff:

  Correlation threshold for an edge.

## Value

A list with `cor` (the K×K correlation matrix) and `posadj` (the
thresholded positive adjacency).
