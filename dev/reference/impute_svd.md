# SVD Imputation

A wrapper around the
[`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).
Impute missing values using singular value decomposition (SVD)
imputation. SVD is a matrix factorization technique that factors a
matrix into three matrices: U, Σ, and V. SVD is used to find the best
lower rank approximation of the original matrix.

## Usage

``` r
impute_svd(x, by = NULL, ...)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- ...:

  Additional arguments to pass to
  [`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed.
