# Half Sample Minimum Imputation

Impute missing values with half of the minimum value of the
corresponding sample. This method assumes that missing values are MCAR,
i.e. missing values are induced by an ion below the detection limit.
Compared to
[`impute_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_sample_min.md),
this method is more conservative.

## Usage

``` r
impute_half_sample_min(x)
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
