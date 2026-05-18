# Minimum Probability Imputation

Impute missing values using random draws from the left-censored gaussian
distribution. Missing values are imputed on the log2 intensity scale and
then transformed back to the original scale.

## Usage

``` r
impute_min_prob(x, by = NULL, q = 0.01, tune.sigma = 1, ...)

# S3 method for class 'glyexp_experiment'
impute_min_prob(x, by = NULL, q = 0.01, tune.sigma = 1, ...)

# S3 method for class 'matrix'
impute_min_prob(x, by = NULL, q = 0.01, tune.sigma = 1, ...)

# Default S3 method
impute_min_prob(x, by = NULL, q = 0.01, tune.sigma = 1, ...)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- q:

  Quantile used to estimate the lower-intensity center for each sample.
  Default is `0.01`.

- tune.sigma:

  Non-negative multiplier for the standard deviation of the
  left-censored draw distribution. Default is `1`.

- ...:

  Reserved for backward compatibility. Extra arguments are not
  supported.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
