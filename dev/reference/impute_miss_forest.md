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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

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

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed. Glyco SE inputs return the same
subclass.
