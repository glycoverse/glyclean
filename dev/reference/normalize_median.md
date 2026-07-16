# Median Normalization

Normalize the expression matrix by dividing each column (sample) by the
median of that column (NA ignored), so that the median of each column
is 1. This is the most common normalization method for proteomics data.
It effectively and robustly removes the bias introduced by total protein
abundance, and removes batch effects in part.

## Usage

``` r
normalize_median(x)
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
