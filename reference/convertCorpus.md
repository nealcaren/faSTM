# Convert documents/vocab between corpus formats (stm-compatible)

Port of `stm:::convertCorpus`. `"Matrix"` returns a documents x V sparse
dgCMatrix; `"lda"` returns the documents list (the lda/stm format).

## Usage

``` r
convertCorpus(documents, vocab, type = c("Matrix", "lda", "slam"))
```

## Arguments

- documents:

  stm-style documents list.

- vocab:

  Vocabulary vector.

- type:

  `"Matrix"` or `"lda"`.

## Value

The corpus in the requested format.
