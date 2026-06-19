# Fit a structural topic model and return its raw arrays.

Inputs are pre-converted in `R/stm.R`:

- `docs_flat` / `doc_lens`: documents as one concatenated 0-based
  token-id stream plus per-document lengths (counts already expanded).
  Reassembled here into `Vec<Vec<u32>>`, the shape `fit_ctm` wants.

- `prevalence`: row-major D×P design matrix flattened (NULL if none).

- `content_groups`: per-doc 0-based group id (NULL if none);
  `num_groups`.

## Usage

``` r
fit_stm(
  docs_flat,
  doc_lens,
  num_types,
  num_topics,
  em_iters,
  em_tol,
  sigma_shrink,
  prevalence,
  num_features,
  content_groups,
  num_groups,
  init_spectral,
  init_beta,
  gamma_l1_alpha,
  diagonal,
  seed,
  inference,
  batch_size,
  tau,
  kappa,
  num_threads
)
```

## Details

`inference`: "batch" -\> `fit_ctm` (parity-validated). "svi" -\>
`fit_ctm_svi` once topica \#231 PR B (STM-SVI) is in the pinned
revision; the R layer gates the prevalence/content + svi combination
until then.
