# Zero Imputation

Impute missing values in an expression matrix by replacing them with
zeros.

## Usage

``` r
impute_zero(x)

# S3 method for class 'glyexp_experiment'
impute_zero(x)

# S3 method for class 'matrix'
impute_zero(x)

# Default S3 method
impute_zero(x)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
