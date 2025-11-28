# Total Area Normalization

Normalize the expression matrix by dividing each column (sample) by the
sum of that column, so that the sum of each column is 1. This is the
most common normalization method for glycomics data. It removes the bias
introduced by total glycan abundance. However, it results in
compositional data, which may result in unrealistic downstream analysis
results.

## Usage

``` r
normalize_total_area(x)

# S3 method for class 'glyexp_experiment'
normalize_total_area(x)

# S3 method for class 'matrix'
normalize_total_area(x)

# Default S3 method
normalize_total_area(x)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.
