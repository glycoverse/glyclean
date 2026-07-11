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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
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

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix. SummarizedExperiment inputs
return the same class.
