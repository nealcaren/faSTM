# Find topics whose top words include given words

Find topics whose top words include given words

## Usage

``` r
find_topic(model, words, n = 20L, type = c("prob", "frex", "lift", "score"))
```

## Arguments

- model:

  A faSTM fit.

- words:

  Character vector of query words.

- n:

  Top words per topic to search.

- type:

  Ranking metric: `"prob"`, `"frex"`, `"lift"`, or `"score"`.

## Value

Integer vector of matching topics.
