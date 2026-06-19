# Create a held-out version of a corpus for document-completion validation

Create a held-out version of a corpus for document-completion validation

## Usage

``` r
make_heldout(
  corpus,
  N = floor(0.1 * length(corpus$documents)),
  proportion = 0.5,
  seed = NULL
)
```

## Arguments

- corpus:

  A `faSTM_corpus`.

- N:

  Number of documents to hold tokens out of (default: 10% of docs).

- proportion:

  Fraction of each chosen document's term *types* to hold out.

- seed:

  Optional RNG seed.

## Value

A list with `corpus` (training corpus, held-out tokens removed) and
`missing` (per-document held-out terms + counts), class `faSTM_heldout`.
