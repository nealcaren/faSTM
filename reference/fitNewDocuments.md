# Infer topics for new documents (stm-compatible signature)

Drop-in for
[`stm::fitNewDocuments()`](https://rdrr.io/pkg/stm/man/fitNewDocuments.html).
Holds the fitted topics fixed and runs the variational E-step for each
new document. Supports stm's prior modes and posterior return.

## Usage

``` r
fitNewDocuments(
  model,
  documents,
  newData = NULL,
  origData = NULL,
  prevalence = NULL,
  betaIndex = NULL,
  prevalencePrior = c("Average", "Covariate", "None"),
  contentPrior = c("Covariate", "Average"),
  returnPosterior = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  A faSTM fit.

- documents:

  New documents: a `faSTM_corpus`/`dfm`/matrix (aligned to the model
  vocabulary), or an stm-style list of 2 x n integer matrices indexed
  into `model$vocab`.

- newData, origData:

  Covariate frames for the new and original documents (used by
  `prevalencePrior = "Covariate"` to set each document's prior mean).

- prevalence:

  Prevalence formula (same RHS as the fit) for the covariate prior.

- betaIndex:

  Integer per-document content-group index (content models).

- prevalencePrior:

  `"Average"` (global prior mean, default) or `"Covariate"`
  (per-document mean from `prevalence`/`newData`).

- contentPrior:

  `"Covariate"` (use the group's topic-word matrix via `betaIndex`,
  default) or `"Average"` (group-marginal).

- returnPosterior:

  If `TRUE`, return `list(theta, eta, nu)` (per-document variational
  mean and Laplace covariance); otherwise a documents x K theta matrix.

- verbose:

  Logical.

- ...:

  Ignored (stm signature compatibility).

## Value

A theta matrix, or a posterior list when `returnPosterior = TRUE`.
