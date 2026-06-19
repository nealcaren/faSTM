# Read/write a corpus in LDA-C (Blei) sparse format

Each line is `M term:count term:count ...` with 0-based term ids.

## Usage

``` r
read_ldac(file)

write_ldac(documents, file)
```

## Arguments

- file:

  Path to the `.ldac`/`.dat` file (read) or output path (write).

- documents:

  A list of 2×n integer matrices (1-based ids).

## Value

`read_ldac` returns a list of 2×n integer matrices (faSTM/stm document
format, 1-based ids); `write_ldac` returns the path invisibly.
