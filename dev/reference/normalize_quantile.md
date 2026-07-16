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
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

- ...:

  Additional arguments to pass to
  [`limma::normalizeQuantiles()`](https://rdrr.io/pkg/limma/man/normalizequantiles.html).

## Value

A container of the same class as `x`, with a normalized expression
matrix.
