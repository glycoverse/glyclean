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

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed. SummarizedExperiment inputs return
the same class.
