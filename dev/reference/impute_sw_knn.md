# Sample-wise KNN Imputation

A wrapper around the
[`impute::impute.knn()`](https://rdrr.io/pkg/impute/man/impute.knn.html).
Impute missing values with values from the k-nearest neighbors of the
corresponding sample. If there are strong patterns among the samples
(such as group clustering relationships or experimental conditions),
this method can better utilize the overall relationships among samples.

## Usage

``` r
impute_sw_knn(x, k = 5, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
impute_sw_knn(x, k = 5, by = NULL, ...)

# S3 method for class 'matrix'
impute_sw_knn(x, k = 5, by = NULL, ...)

# Default S3 method
impute_sw_knn(x, k = 5, by = NULL, ...)
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
