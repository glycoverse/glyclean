# MissForest Imputation

A wrapper around the
[`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html).
Impute missing values using recursive running of random forests until
convergence. This is a non-parametric method and works for both MAR and
MNAR missing data.

## Usage

``` r
impute_miss_forest(x, by = NULL, seed = 123, ...)

# S3 method for class 'glyexp_experiment'
impute_miss_forest(x, by = NULL, seed = 123, ...)

# S3 method for class 'matrix'
impute_miss_forest(x, by = NULL, seed = 123, ...)

# Default S3 method
impute_miss_forest(x, by = NULL, seed = 123, ...)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- seed:

  Integer seed for random number generation. Default is 123.

- ...:

  Additional arguments to pass to
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
