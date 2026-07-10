# Sample Minimum Imputation

Impute missing values with the minimum value of the corresponding
sample. This method assumes that missing values are MCAR, i.e. missing
values are induced by an ion below the detection limit. See also
[`impute_half_sample_min()`](https://glycoverse.github.io/glyclean/dev/reference/impute_half_sample_min.md).

## Usage

``` r
impute_sample_min(x)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed.
