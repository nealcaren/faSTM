# Fit a structural topic model (fast Rust backend, stm-compatible object)

A drop-in replacement for
[`stm::stm()`](https://rdrr.io/pkg/stm/man/stm.html)'s fitting step.
Accepts the same `documents` / `vocab` / `prevalence` / `content`
inputs, fits with topica's Rust core, and returns an object compatible
with the `stm` package so that
[`stm::labelTopics()`](https://rdrr.io/pkg/stm/man/labelTopics.html),
[`stm::plot.STM()`](https://rdrr.io/pkg/stm/man/plot.STM.html),
[`stm::findThoughts()`](https://rdrr.io/pkg/stm/man/findThoughts.html),
[`stm::sageLabels()`](https://rdrr.io/pkg/stm/man/sageLabels.html), and
[`stm::toLDAvis()`](https://rdrr.io/pkg/stm/man/toLDAvis.html) work
unmodified. Use
[`estimateEffect()`](https://nealcaren.github.io/faSTM/reference/estimateEffect.md)
from this package for the honest covariate effects.

## Usage

``` r
stm(
  documents,
  vocab,
  K,
  prevalence = NULL,
  content = NULL,
  data = NULL,
  max.em.its = 500L,
  emtol = 1e-05,
  init.type = c("Spectral", "Random", "LDA", "Custom"),
  init.beta = NULL,
  model = NULL,
  gamma.prior = c("Pooled", "L1"),
  gamma.l1.alpha = 0.001,
  sigma.prior = 0,
  seed = 1L,
  inference = c("batch", "svi"),
  batch_size = 256L,
  tau = 64,
  kappa = 0.7,
  num_threads = 0L,
  verbose = TRUE,
  ...
)
```

## Arguments

- documents:

  stm-format documents: a named list of `2 x n_d` integer matrices (row
  1 = 1-based word id into `vocab`, row 2 = count). Produced by
  [`stm::prepDocuments()`](https://rdrr.io/pkg/stm/man/prepDocuments.html).

- vocab:

  Character vector of vocabulary terms.

- K:

  Number of topics.

- prevalence:

  A right-hand-side formula (e.g. `~ treatment + s(age)`) or a design
  matrix; topic prevalence covariates. `data` supplies the variables.

- content:

  A right-hand-side formula naming a single categorical variable, or a
  factor; the SAGE content covariate. `data` supplies the variable.

- data:

  A data.frame of document metadata (the `meta` from
  [`stm::prepDocuments()`](https://rdrr.io/pkg/stm/man/prepDocuments.html)),
  aligned to `documents`.

- max.em.its:

  Maximum EM iterations (batch) / epochs (svi).

- emtol:

  Relative-bound convergence tolerance.

- init.type:

  Topic initialization: `"Spectral"` (stm's default), `"Random"`,
  `"LDA"` (seed from a quick CVB0 LDA, like stm's collapsed-Gibbs init),
  or `"Custom"` (seed from `init.beta` or a supplied `model`).

- init.beta:

  Optional K x V topic-word probability matrix to start the fit from a
  given initialization (overrides `init.type`). Supplying R `stm`'s
  exact spectral beta here reproduces that run — a guaranteed "replicate
  the original" mode (topica \#234/#235).

- model:

  A fitted model whose topic-word matrix seeds `init.type = "Custom"`.

- gamma.prior:

  Prevalence-coefficient prior: `"Pooled"` (ridge, stm default) or
  `"L1"`.

- sigma.prior:

  Shrinkage applied to the topic covariance off-diagonal.

- seed:

  Integer seed (batch fit is reproducible from it).

- inference:

  `"batch"` (default, parity-validated) or `"svi"` (stochastic
  variational; scales to large corpora — requires a topica build with
  STM-SVI).

- batch_size, tau, kappa:

  SVI controls (minibatch size; Robbins-Monro `(tau + t)^(-kappa)` step
  schedule). Ignored when `inference = "batch"`.

- num_threads:

  Worker threads for the parallel variational E-step. `0` (default) uses
  all cores; `>= 1` pins a scoped pool. Results are identical regardless
  of thread count.

- verbose:

  Logical; print progress.

## Value

An object of class `c("faSTM", "STM")` — an stm-compatible fit.
