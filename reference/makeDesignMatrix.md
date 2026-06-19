# Build a (sparse) design matrix for new data (stm-compatible)

Port of `stm:::makeDesignMatrix`: builds the model matrix for `newData`
using the term structure and factor levels of `origData`.

## Usage

``` r
makeDesignMatrix(formula, origData, newData, sparse = TRUE, ...)
```

## Arguments

- formula:

  A model formula.

- origData:

  Data defining the terms/levels.

- newData:

  Data to build the matrix for.

- sparse:

  Return a sparse matrix.

- ...:

  Ignored.

## Value

A (sparse) design matrix.
