# Build a faSTM corpus from prepared text

faSTM does not do its own tokenization — it reads an already-prepared
document-term representation from the tools the field already uses
(`quanteda`, `tidytext`) or a plain sparse matrix. `as_corpus()`
normalizes any of these into the structure
[`stm()`](https://nealcaren.github.io/faSTM/reference/stm.md) consumes,
dropping empty documents and re-indexing the vocabulary, with metadata
kept aligned.

## Usage

``` r
as_corpus(x, meta = NULL, ...)
```

## Arguments

- x:

  A `quanteda` `dfm`, a document-term `Matrix`/matrix (documents in
  rows, terms in columns, with `colnames`), or an existing
  `faSTM_corpus`. For a tidy (long) term table use
  [`from_tidy()`](https://nealcaren.github.io/faSTM/reference/from_tidy.md).

- meta:

  Optional data.frame of document metadata, one row per document,
  aligned to `x`. For a `dfm`, defaults to `quanteda::docvars(x)`.

- ...:

  Unused.

## Value

A `faSTM_corpus`: a list with `documents` (named list of 2×n integer
matrices: row 1 = 1-based term id, row 2 = count), `vocab` (character),
`meta` (data.frame or NULL), and `word_counts` (corpus term
frequencies).
