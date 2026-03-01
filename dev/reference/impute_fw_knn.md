# Feature-wise KNN Imputation

A wrapper around the
[`impute::impute.knn()`](https://rdrr.io/pkg/impute/man/impute.knn.html).
Impute missing values with values from the k-nearest neighbors of the
corresponding feature.

## Usage

``` r
impute_fw_knn(x, k = 5, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
impute_fw_knn(x, k = 5, by = NULL, ...)

# S3 method for class 'matrix'
impute_fw_knn(x, k = 5, by = NULL, ...)

# Default S3 method
impute_fw_knn(x, k = 5, by = NULL, ...)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- k:

  The number of nearest neighbors to consider.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- ...:

  Additional arguments to pass to
  [`impute::impute.knn()`](https://rdrr.io/pkg/impute/man/impute.knn.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
