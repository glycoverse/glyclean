# LoessF Normalization

This function is a wrapper around
[`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html)
with `method = "fast"`. Each column is simply normalized to a reference
array, the reference array being the average of all the arrays. See
[this paper](https://doi.org/10.1093/bib/bbw095) for more information.
Also see
[`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html).

## Usage

``` r
normalize_loessf(x, by = NULL, ...)

# S3 method for class 'glyexp_experiment'
normalize_loessf(x, by = NULL, ...)

# S3 method for class 'matrix'
normalize_loessf(x, by = NULL, ...)

# Default S3 method
normalize_loessf(x, by = NULL, ...)
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
  [`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with normalized expression matrix. If `x`
is a matrix, returns a normalized matrix.
