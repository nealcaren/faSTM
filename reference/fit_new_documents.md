# Infer topic proportions for new documents

Runs the variational E-step for each new document against the fitted
model's fixed global parameters (topic-word matrix, prior mean and
covariance), giving out-of-sample topic proportions (cf.
[`stm::fitNewDocuments`](https://rdrr.io/pkg/stm/man/fitNewDocuments.html)).
The model's topics are held fixed; only each new document's proportions
are estimated.

## Usage

``` r
fit_new_documents(model, newdata)
```

## Arguments

- model:

  A faSTM fit (non-content; for content models the group-marginal
  topic-word matrix is used, with a warning).

- newdata:

  A `faSTM_corpus`, quanteda `dfm`, or document-term matrix. Terms are
  aligned to the model's vocabulary; out-of-vocabulary terms are
  dropped.

## Value

A new-documents × K matrix of topic proportions.
