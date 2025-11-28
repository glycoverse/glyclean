# Robust Linear Regression with Median Adjustment Normalization

This method is based on robust linear regression with median adjustment.
First, the median of each variable's abundance across all measured
samples is subtracted from the sample's abundance values. Then, it's
like the
[`normalize_rlr()`](https://glycoverse.github.io/glyclean/reference/normalize_rlr.md)
method.

## Usage

``` r
normalize_rlrma(x, by = NULL)

# S3 method for class 'glyexp_experiment'
normalize_rlrma(x, by = NULL)

# S3 method for class 'matrix'
normalize_rlrma(x, by = NULL)

# Default S3 method
normalize_rlrma(x, by = NULL)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.
