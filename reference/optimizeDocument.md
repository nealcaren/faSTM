# Per-document variational E-step (stm-compatible)

Port of `stm:::optimizeDocument`'s interface: infers one document's
topic proportions against fixed globals and returns its variational mean
`lambda` (eta), Laplace covariance `nu`, and `theta`.

## Usage

``` r
optimizeDocument(document, eta, mu, beta, sigma = NULL, sigmainv = NULL, ...)
```

## Arguments

- document:

  A 2 x n integer matrix (1-based vocab ids; counts).

- eta:

  Ignored starting value (kept for signature compatibility).

- mu:

  Prior mean (length K-1).

- beta:

  K x V topic-word probability matrix.

- sigma, sigmainv:

  Prior covariance or its inverse (supply one).

- ...:

  Ignored (stm signature compatibility).

## Value

A list with `lambda`, `nu`, and `theta`.
