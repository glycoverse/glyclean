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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

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

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed. Glyco SE inputs return the same
subclass.
