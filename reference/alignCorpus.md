# Align a new corpus to a reference vocabulary (stm-compatible)

stm-shaped counterpart to
[`align_corpus()`](https://nealcaren.github.io/faSTM/reference/align_corpus.md):
reindexes `new`'s documents onto `old.vocab`, dropping out-of-vocabulary
terms (and empty documents).

## Usage

``` r
alignCorpus(new, old.vocab, verbose = TRUE)
```

## Arguments

- new:

  An stm-style `list(documents, vocab)` or a `faSTM_corpus`.

- old.vocab:

  Reference vocabulary to align onto.

- verbose:

  Logical.

## Value

`list(documents, vocab, docs.removed, words.removed)`.
