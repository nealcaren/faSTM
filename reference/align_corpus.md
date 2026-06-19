# Align a new corpus to a fitted model's vocabulary

Maps a new corpus's terms onto the term indices of a fitted faSTM model,
dropping out-of-vocabulary terms — the preprocessing needed before
inferring topics for new documents (cf.
[`stm::alignCorpus`](https://rdrr.io/pkg/stm/man/alignCorpus.html)).

## Usage

``` r
align_corpus(newdata, model)
```

## Arguments

- newdata:

  A `faSTM_corpus`, quanteda `dfm`, or document-term matrix.

- model:

  A faSTM fit.

## Value

A list with per-document `ids` (0-based indices into `model$vocab`) and
`counts`, plus `dropped` (count of out-of-vocabulary term tokens).
