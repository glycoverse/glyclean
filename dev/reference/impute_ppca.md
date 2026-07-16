# PPCA Imputation

A wrapper around the
[`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).
Impute missing values using probabilistic principal component analysis
(PPCA). PPCA allows to perform PCA on incomplete data and may be used
for missing value estimation.

## Usage

``` r
impute_ppca(x, by = NULL, ...)
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
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- ...:

  Additional arguments to pass to
  [`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).

## Value

A container of the same class as `x`, with missing values imputed.
