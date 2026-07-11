# Minimum Probability Imputation

Impute missing values using random draws from the left-censored gaussian
distribution. Missing values are imputed on the log2 intensity scale and
then transformed back to the original scale.

## Usage

``` r
impute_min_prob(x, by = NULL, q = 0.01, tune.sigma = 1, ...)
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

- q:

  Quantile used to estimate the lower-intensity center for each sample.
  Default is `0.01`.

- tune.sigma:

  Non-negative multiplier for the standard deviation of the
  left-censored draw distribution. Default is `1`.

- ...:

  Reserved for backward compatibility. Extra arguments are not
  supported.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed. SummarizedExperiment inputs return
the same class.
