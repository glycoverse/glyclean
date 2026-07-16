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
