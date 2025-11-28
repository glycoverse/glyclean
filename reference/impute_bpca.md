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

# S3 method for class 'glyexp_experiment'
impute_bpca(x, by = NULL, ...)

# S3 method for class 'matrix'
impute_bpca(x, by = NULL, ...)

# Default S3 method
impute_bpca(x, by = NULL, ...)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Used for grouping when
  imputing missing values.

- ...:

  Additional arguments to pass to
  [`pcaMethods::pca()`](https://rdrr.io/pkg/pcaMethods/man/pca.html).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
