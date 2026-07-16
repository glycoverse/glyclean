# Absolute Median Normalization

Normalize the expression matrix by dividing each column (sample) by the
absolute median of that column (NA ignored), so that the absolute median
of each column is 1.

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
