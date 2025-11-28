# Robust Linear Regression Normalization

This method is based on robust linear regression. The reference sample
is calculated as the median value of each variable's abundance across
all measured samples. For each sample, a robust linear regression model
is fitted to the sample's abundance values against the reference
sample's abundance values. The fitted model is then used to normalize
the sample's abundance values. The underlying assumption is that the
diﬀerent intensities observed across individuals are imputable to
diﬀerent amounts of the biological material in the collected samples.

## Usage

``` r
normalize_rlr(x, by = NULL)

# S3 method for class 'glyexp_experiment'
normalize_rlr(x, by = NULL)

# S3 method for class 'matrix'
normalize_rlr(x, by = NULL)

# Default S3 method
normalize_rlr(x, by = NULL)
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
