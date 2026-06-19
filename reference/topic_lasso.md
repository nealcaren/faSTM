# Predict a document-level outcome from topic proportions (lasso)

Cross-validated lasso (`glmnet`) of an outcome on the topic-proportion
matrix (cf.
[`stm::topicLasso`](https://rdrr.io/pkg/stm/man/topicLasso.html)).
Identifies which topics predict the outcome.

## Usage

``` r
topic_lasso(
  formula,
  model,
  data,
  family = "gaussian",
  nfolds = 10L,
  seed = 2138L,
  ...
)
```

## Arguments

- formula:

  `outcome ~ .` — the LHS names the outcome in `data`.

- model:

  A faSTM fit (supplies the topic proportions).

- data:

  Document-level data with the outcome, aligned to the documents.

- family:

  glmnet family (`"gaussian"`, `"binomial"`, ...).

- nfolds:

  CV folds.

- seed:

  RNG seed.

- ...:

  Passed to
  [`glmnet::cv.glmnet()`](https://glmnet.stanford.edu/reference/cv.glmnet.html).

## Value

A `faSTM_topiclasso` with selected per-topic coefficients.
