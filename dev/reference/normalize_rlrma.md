# Robust Linear Regression with Median Adjustment Normalization

This method is based on robust linear regression with median adjustment.
First, the median of each variable's abundance across all measured
samples is subtracted from the sample's abundance values. Then, it's
like the
[`normalize_rlr()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_rlr.md)
method.

## Usage

``` r
normalize_rlrma(x, by = NULL)
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
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix. SummarizedExperiment inputs
return the same class.
