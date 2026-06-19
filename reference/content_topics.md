# Marginal content words by one content covariate

For a multi-covariate (crossed) content model, recovers the topic-word
labels for each level of a single content covariate, averaging the
crossed topic-word distributions over the other covariate(s). Lets you
read off how topics' vocabulary shifts with one covariate while
marginalizing the rest.

## Usage

``` r
content_topics(model, by = NULL, n = 7L, type = c("prob", "lift", "frex"))
```

## Arguments

- model:

  A content (SAGE) faSTM fit.

- by:

  Content covariate name to marginalize *to* (default: the first).

- n:

  Words per topic.

- type:

  `"prob"`, `"lift"`, or `"frex"`.

## Value

A named list (one entry per level of `by`) of K x `n` word matrices.
