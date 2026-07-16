# MissForest Imputation

A wrapper around the
[`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html).
Impute missing values using recursive running of random forests until
convergence. This is a non-parametric method and works for both MAR and
MNAR missing data.

## Usage

``` r
impute_miss_forest(x, by = NULL, seed = 123, ...)
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

- seed:

  Integer seed for random number generation. Default is 123.

- ...:

  Additional arguments to pass to
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html).

## Value

A container of the same class as `x`, with missing values imputed.
