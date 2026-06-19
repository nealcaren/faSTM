# Coerce inputs into an stm-style corpus (stm-compatible)

Port of
[`stm::asSTMCorpus`](https://rdrr.io/pkg/stm/man/asSTMCorpus.html)'s
role: accepts a `faSTM_corpus`, quanteda `dfm`, or document-term matrix
and returns `list(documents, vocab, data)` in stm format.

## Usage

``` r
asSTMCorpus(documents, vocab = NULL, data = NULL, ...)
```

## Arguments

- documents:

  A corpus/dfm/matrix, or an stm-style documents list.

- vocab:

  Vocabulary (when `documents` is already a documents list).

- data:

  Optional metadata.

- ...:

  Ignored.

## Value

`list(documents, vocab, data)`.
