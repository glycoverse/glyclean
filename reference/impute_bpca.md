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
impute_bpca(x, by = NULL, ..., seed = 123)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
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

- seed:

  Non-negative integer seed for random number generation. Default is
  `123`. The caller's random state is restored, including on errors.

## Value

A container of the same class as `x`, with missing values imputed.

## See also

[data_scales](https://glycoverse.github.io/glyclean/reference/data_scales.md)
for working domains and supported input scales.
