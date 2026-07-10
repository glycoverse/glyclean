# Sample-wise KNN Imputation

A wrapper around the
[`impute::impute.knn()`](https://rdrr.io/pkg/impute/man/impute.knn.html).
Impute missing values with values from the k-nearest neighbors of the
corresponding sample. If there are strong patterns among the samples
(such as group clustering relationships or experimental conditions),
this method can better utilize the overall relationships among samples.

## Usage

``` r
impute_sw_knn(x, k = 5, by = NULL, ...)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
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

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed.
