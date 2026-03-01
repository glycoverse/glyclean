# SVD Imputation

A wrapper around the
[`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).
Impute missing values using singular value decomposition (SVD)
imputation. SVD is a matrix factorization technique that factors a
matrix into three matrices: U, Σ, and V. SVD is used to find the best
lower rank approximation of the original matrix.

## Usage

``` r
impute_svd(x, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
impute_svd(x, by = NULL, ...)

# S3 method for class 'matrix'
impute_svd(x, by = NULL, ...)

# Default S3 method
impute_svd(x, by = NULL, ...)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- ...:

  Additional arguments to pass to
  [`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
