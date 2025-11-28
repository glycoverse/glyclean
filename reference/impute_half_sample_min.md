# Half Sample Minimum Imputation

Impute missing values with half of the minimum value of the
corresponding sample. This method assumes that missing values are MCAR,
i.e. missing values are induced by an ion below the detection limit.
Compared to
[`impute_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_sample_min.md),
this method is more conservative.

## Usage

``` r
impute_half_sample_min(x)

# S3 method for class 'glyexp_experiment'
impute_half_sample_min(x)

# S3 method for class 'matrix'
impute_half_sample_min(x)

# Default S3 method
impute_half_sample_min(x)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with missing values imputed. If `x` is a
matrix, returns a matrix with missing values imputed.
