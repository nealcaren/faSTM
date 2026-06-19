# Out-of-sample topic inference: for each new document, run the variational E-step against fixed globals (β, μ, Σ⁻¹) and return θ. Documents are passed sparse — `words` are 0-based ids into the *fitted model's* vocabulary (out-of-vocabulary terms dropped by the R caller) with their `counts`, concatenated, plus per-document term counts `doc_nterms`.

Out-of-sample topic inference: for each new document, run the
variational E-step against fixed globals (β, μ, Σ⁻¹) and return θ.
Documents are passed sparse — `words` are 0-based ids into the *fitted
model's* vocabulary (out-of-vocabulary terms dropped by the R caller)
with their `counts`, concatenated, plus per-document term counts
`doc_nterms`.

## Usage

``` r
infer_theta_new(
  beta_flat,
  num_topics,
  num_types,
  mu,
  siginv,
  words,
  counts,
  doc_nterms
)
```
