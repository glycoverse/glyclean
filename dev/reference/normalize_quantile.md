# Quantile Normalization

This function is a wrapper around
[`limma::normalizeQuantiles()`](https://rdrr.io/pkg/limma/man/normalizequantiles.html).
It normalizes the expression matrix by quantile normalization. This
method is used to remove technical variation between samples. Proteomics
data rarely uses this method, but it is common in microarray data. See
[wikipedia](https://en.wikipedia.org/wiki/Quantile_normalization) for
more information.

## Usage

``` r
normalize_quantile(x, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
normalize_quantile(x, by = NULL, ...)

# S3 method for class 'matrix'
normalize_quantile(x, by = NULL, ...)

# Default S3 method
normalize_quantile(x, by = NULL, ...)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

- ...:

  Additional arguments to pass to
  [`limma::normalizeQuantiles()`](https://rdrr.io/pkg/limma/man/normalizequantiles.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.
