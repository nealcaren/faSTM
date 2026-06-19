# Evaluate held-out log-likelihood of a fit on a held-out set

Evaluate held-out log-likelihood of a fit on a held-out set

## Usage

``` r
eval_heldout(model, heldout)
```

## Arguments

- model:

  A faSTM fit (trained on `heldout$corpus`).

- heldout:

  A `faSTM_heldout` (or its `missing` list).

## Value

Mean per-document held-out log-likelihood per token.
