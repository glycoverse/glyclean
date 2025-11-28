# Absolute Median Normalization

Normalize the expression matrix by dividing each column (sample) by the
absolute median of that column (NA ignored), so that the absolute median
of each column is 1.

## Usage

``` r
normalize_median_abs(x)

# S3 method for class 'glyexp_experiment'
normalize_median_abs(x)

# S3 method for class 'matrix'
normalize_median_abs(x)

# Default S3 method
normalize_median_abs(x)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.
