# Variance Stabilizing Normalization

This function is a wrapper around
[`limma::normalizeVSN()`](https://rdrr.io/pkg/limma/man/normalizeVSN.html).
Evidence proved that this method performs well in reducing noise and
boosting differential expression detection. Log-transformation is not
needed for downstream statistical analysis, for this normalization
method performs a log-like transformation internally. Due to the same
reason, fold change estimates will be severely distorted. Please use
this method with caution. See [this
paper](https://doi.org/10.1093/bib/bbw095) for more information.

## Usage

``` r
normalize_vsn(x, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
normalize_vsn(x, by = NULL, ...)

# S3 method for class 'matrix'
normalize_vsn(x, by = NULL, ...)

# Default S3 method
normalize_vsn(x, by = NULL, ...)
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
  [`limma::normalizeVSN()`](https://rdrr.io/pkg/limma/man/normalizeVSN.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.

## Details

At least 42 variables should be provided for this method. This follows
the convention of the `vsn` package.
