# Dispatch and apply a function by group

This function handles both glyexp_experiment objects and matrices, It
validates input, optionally applies a function to groups of samples, and
returns an object of the same type as the input.

## Usage

``` r
.dispatch_and_apply_by_group(x, matrix_func, by = NULL, ...)
```

## Arguments

- x:

  Input data, either a glyexp_experiment object or a matrix

- matrix_func:

  Function to apply to the matrix (implementation function)

- by:

  Grouping variable for stratified processing

- ...:

  Additional arguments passed to matrix_func

## Value

Same type as input (experiment or matrix)
