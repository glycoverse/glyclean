# BPCA Imputation

A wrapper around the
[`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).
Impute missing values using Bayesian principal component analysis
(BPCA). BPCA combines an EM approach for PCA with a Bayesian model. In
standard PCA data far from the training set but close to the principal
subspace may have the same reconstruction error. BPCA defines a
likelihood function such that the likelihood for data far from the
training set is much lower, even if they are close to the principal
subspace.

## Usage

``` r
impute_bpca(x, by = NULL, ...)
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

- ...:

  Additional arguments to pass to
  [`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed. SummarizedExperiment inputs return
the same class.
