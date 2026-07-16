# Robust Linear Regression with Median Adjustment Normalization

This method is based on robust linear regression with median adjustment.
First, the median of each variable's abundance across all measured
samples is subtracted from the sample's abundance values. Then, it's
like the
[`normalize_rlr()`](https://glycoverse.github.io/glyclean/reference/normalize_rlr.md)
method.

## Usage

``` r
normalize_rlrma(x, by = NULL)
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

## Value

A container of the same class as `x`, with a normalized expression
matrix.
