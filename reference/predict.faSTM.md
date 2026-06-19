# Predict topic proportions for new documents

Predict topic proportions for new documents

## Usage

``` r
# S3 method for class 'faSTM'
predict(object, newdata, ...)
```

## Arguments

- object:

  A faSTM fit.

- newdata:

  New documents (corpus / dfm / matrix / stm-style list).

- ...:

  Passed to
  [`fit_new_documents()`](https://nealcaren.github.io/faSTM/reference/fit_new_documents.md).

## Value

A new-documents x K matrix of topic proportions.
