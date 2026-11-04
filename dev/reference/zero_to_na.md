# Replace Zeros with Missing Values

Replace exact zeros (including negative zero) on `raw`, or `-Inf` on
ordinary log scales, with `NA`. Finite coordinate zero on log scales is
preserved. `logx` is rejected. Other values and existing missing values
are unchanged. This is an explicit preprocessing step: use it only when
zeros represent missing measurements, before choosing an appropriate
imputation method.

## Usage

``` r
zero_to_na(x)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object. Legacy `glyexp_experiment` objects are also supported.

## Value

A container of the same class as `x`, with zeros replaced by `NA`.

## Details

Only the abundance assay is changed. Annotations and metadata are
preserved, with this operation appended to the preprocessing pipeline.
Integer assays retain integer storage. Replacing implicit zeros in a
sparse assay requires storing those entries as missing values and can
increase memory use substantially.

## See also

[`impute_zero()`](https://glycoverse.github.io/glyclean/dev/reference/impute_zero.md),
[`glyclean_pipeline()`](https://glycoverse.github.io/glyclean/dev/reference/glyclean_pipeline.md),
[`get_pipeline()`](https://glycoverse.github.io/glyclean/dev/reference/glyclean_pipeline.md)

[data_scales](https://glycoverse.github.io/glyclean/dev/reference/data_scales.md)
for working domains and supported input scales.
