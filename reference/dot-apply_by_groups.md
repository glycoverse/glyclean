# Apply function with optional grouping

This function applies a matrix function with optional grouping by a
factor/vector.

## Usage

``` r
.apply_by_groups(mat, matrix_func, by_values = NULL, ...)
```

## Arguments

- mat:

  The input matrix

- matrix_func:

  The function to apply to the matrix

- by_values:

  The grouping values (factor or vector), or NULL for no grouping

- ...:

  Additional arguments passed to matrix_func

## Value

Processed matrix
