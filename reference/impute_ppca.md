# PPCA Imputation

A wrapper around the
[`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).
Impute missing values using probabilistic principal component analysis
(PPCA). PPCA allows to perform PCA on incomplete data and may be used
for missing value estimation.

## Usage

``` r
impute_ppca(x, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
impute_ppca(x, by = NULL, ...)

# S3 method for class 'matrix'
impute_ppca(x, by = NULL, ...)

# Default S3 method
impute_ppca(x, by = NULL, ...)
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
