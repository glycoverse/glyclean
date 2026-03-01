# Minimum Probability Imputation

A wrapper around the
[`imputeLCMD::impute.MinProb()`](https://rdrr.io/pkg/imputeLCMD/man/impute.MinProb.html).
Impute missing values using random draws from the left-censored gaussian
distribution.

## Usage

``` r
impute_min_prob(x, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
impute_min_prob(x, by = NULL, ...)

# S3 method for class 'matrix'
impute_min_prob(x, by = NULL, ...)

# Default S3 method
impute_min_prob(x, by = NULL, ...)
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
  [`imputeLCMD::impute.MinProb()`](https://rdrr.io/pkg/imputeLCMD/man/impute.MinProb.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
