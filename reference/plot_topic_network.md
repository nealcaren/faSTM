# Topic correlation network

Nodes are topics (sized by prevalence, labelled by top words); edges
join topics whose proportions are positively correlated above `cutoff`.
Uses a lightweight circular layout — no graph-library dependency.

## Usage

``` r
plot_topic_network(model, cutoff = 0.03, n = 3L, labeltype = "frex")
```

## Arguments

- model:

  A faSTM fit.

- cutoff:

  Correlation threshold for an edge.

- n:

  Top words per topic label.

- labeltype:

  Word ranking for labels.

## Value

A ggplot object.
