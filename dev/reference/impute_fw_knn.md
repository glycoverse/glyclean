# Feature-wise KNN Imputation

A wrapper around the
[`impute::impute.knn()`](https://rdrr.io/pkg/impute/man/impute.knn.html).
Impute missing values with values from the k-nearest neighbors of the
corresponding feature.

## Usage

``` r
impute_fw_knn(x, k = 5, by = NULL, ...)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- k:

  The number of nearest neighbors to consider.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- ...:

  Additional arguments to pass to
  [`impute::impute.knn()`](https://rdrr.io/pkg/impute/man/impute.knn.html).

## Value

A container of the same class as `x`, with missing values imputed.
