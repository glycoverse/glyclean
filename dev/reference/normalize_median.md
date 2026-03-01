# Median Normalization

Normalize the expression matrix by dividing each column (sample) by the
median of that column (NA ignored), so that the median of each column
is 1. This is the most common normalization method for proteomics data.
It effectively and robustly removes the bias introduced by total protein
abundance, and removes batch effects in part.

## Usage

``` r
normalize_median(x)

# S3 method for class 'glyexp_experiment'
normalize_median(x)

# S3 method for class 'matrix'
normalize_median(x)

# Default S3 method
normalize_median(x)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.
