# Absolute Median Normalization

Normalize the expression matrix by dividing each column (sample) by the
absolute median of that column (NA ignored), retaining the geometric
mean of the absolute column medians.

## Usage

``` r
normalize_median_abs(x)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

## Value

A container of the same class as `x`, with a normalized expression
matrix.

## See also

[data_scales](https://glycoverse.github.io/glyclean/dev/reference/data_scales.md)
for working domains and supported input scales.
