# Dispatch on input type

This function provides a basic dispatch mechanism to handle both
glyexp_experiment objects and matrices. It's a simpler alternative to
[`.dispatch_and_apply_by_group()`](https://glycoverse.github.io/glyclean/reference/dot-dispatch_and_apply_by_group.md)
for functions that do not need automatic group-wise application.

## Usage

``` r
.dispatch_on_input(x, fun_exp, fun_mat, ...)
```

## Arguments

- x:

  Input data, either a glyexp_experiment object or a matrix.

- fun_exp:

  Function to execute for glyexp_experiment input.

- fun_mat:

  Function to execute for matrix input.

- ...:

  Additional arguments passed to `fun_exp` and `fun_mat`.

## Value

The result of either `fun_exp` or `fun_mat`.
