# Robust Linear Regression with Median Adjustment and Cyclic Normalization

This method is based on robust linear regression with median adjustment
and cyclic normalization. The method is applied iteratively to each pair
of samples. For each pair of samples, the median of the differences
between the two samples is calculated. A robust linear regression model
is fitted to the differences against the averages of the two samples.
The fitted model is then used to normalize the two samples. The process
is repeated for a number of iterations.

## Usage

``` r
normalize_rlrmacyc(x, n_iter = 3, by = NULL)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- n_iter:

  The number of iterations to perform. Default is 3.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix. SummarizedExperiment inputs
return the same class.
