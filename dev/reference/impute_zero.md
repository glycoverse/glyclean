# Zero Imputation

Impute missing values in an expression matrix by replacing them with
zeros. Only `glyexp` experiments with `scale = "raw"` are supported.

## Usage

``` r
impute_zero(x)
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

A container of the same class as `x`, with missing values imputed.

## See also

[data_scales](https://glycoverse.github.io/glyclean/dev/reference/data_scales.md)
for working domains and supported input scales.
