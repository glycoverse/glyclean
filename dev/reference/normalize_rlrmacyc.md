# Robust Linear Regression with Median Adjustment and Cyclic Normalization

This method is based on robust linear regression with median adjustment
and cyclic normalization. The method is applied iteratively to each pair
of samples. For each pair of samples, the median of the differences
between the two samples is calculated. A robust linear regression model
is fitted to the differences against the averages of the two samples.
The fitted model is then used to normalize the two samples. The process
is repeated for a number of iterations.

## Usage

``` r
normalize_rlrmacyc(x, n_iter = 3, by = NULL)

# S3 method for class 'glyexp_experiment'
normalize_rlrmacyc(x, n_iter = 3, by = NULL)

# S3 method for class 'matrix'
normalize_rlrmacyc(x, n_iter = 3, by = NULL)

# Default S3 method
normalize_rlrmacyc(x, n_iter = 3, by = NULL)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- n_iter:

  The number of iterations to perform. Default is 3.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.
