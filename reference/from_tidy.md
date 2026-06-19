# Build a faSTM corpus from a tidy (long) term-count table

For `tidytext`-style data: one row per (document, term) with a count.

## Usage

``` r
from_tidy(data, document = "document", term = "term", count = "n", meta = NULL)
```

## Arguments

- data:

  A data.frame.

- document, term, count:

  Column names (strings) for the document id, the term, and the count.
  `count` defaults to a count of rows per (doc, term).

- meta:

  Optional per-document metadata, aligned to the sorted unique
  documents.

## Value

A `faSTM_corpus`.
