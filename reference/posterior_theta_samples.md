# Draw from the per-document topic-proportion posterior

The variational (Laplace) posterior of each document's logit-topic
vector is `eta_d ~ N(lambda_d, nu_d)`, both stored on a faSTM fit. This
draws `nsims` samples of theta per document by sampling eta and applying
the softmax (with the reference topic appended as 0). This is the pure-R
equivalent of topica's `posterior_theta_samples`; no Rust call is needed
because `eta` + `nu` fully describe the posterior. Feeds
[`estimateEffect()`](https://nealcaren.github.io/faSTM/reference/estimateEffect.md)'s
method of composition.

## Usage

``` r
posterior_theta_samples(model, nsims = 100L, seed = NULL)
```

## Arguments

- model:

  A faSTM fit (from
  [`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md)).

- nsims:

  Number of posterior draws.

- seed:

  Optional integer seed for reproducible draws.

## Value

A `nsims`-length list of `D x K` theta matrices.
