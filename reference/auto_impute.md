# Automatic Imputation

This function automatically selects and applies the most suitable
imputation method for the given dataset. If Quality Control (QC) samples
are present, the method that best stabilizes them (i.e., yields the
lowest median coefficient of variation) is chosen. Otherwise, it
defaults to a sample-size-based strategy:

- less than 30 samples: Sample minimum imputation

- between 30 and 100 samples: Minimum probability imputation

- more than 100 samples: MissForest imputation

## Usage

``` r
auto_impute(
  exp,
  group_col = "group",
  qc_name = "QC",
  to_try = NULL,
  info = NULL
)
```

## Arguments

- exp:

  An
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html).

- group_col:

  The column name in sample_info for groups. Default is "group". Can be
  NULL when no group information is available.

- qc_name:

  The name of QC samples in the `group_col` column. Default is "QC".
  Only used when `group_col` is not NULL. Can be NULL when no QC samples
  are available.

- to_try:

  Imputation functions to try. A list. Default includes:

  - [`impute_zero()`](https://glycoverse.github.io/glyclean/reference/impute_zero.md):
    zero imputation

  - [`impute_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_sample_min.md):
    sample minimum imputation

  - [`impute_half_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_half_sample_min.md):
    half sample minimum imputation

  - [`impute_sw_knn()`](https://glycoverse.github.io/glyclean/reference/impute_sw_knn.md):
    sample-wise KNN imputation

  - [`impute_fw_knn()`](https://glycoverse.github.io/glyclean/reference/impute_fw_knn.md):
    feature-wise KNN imputation

  - [`impute_bpca()`](https://glycoverse.github.io/glyclean/reference/impute_bpca.md):
    BPCA imputation

  - [`impute_ppca()`](https://glycoverse.github.io/glyclean/reference/impute_ppca.md):
    PPCA imputation

  - [`impute_svd()`](https://glycoverse.github.io/glyclean/reference/impute_svd.md):
    SVD imputation

  - [`impute_min_prob()`](https://glycoverse.github.io/glyclean/reference/impute_min_prob.md):
    minimum probability imputation

  - [`impute_miss_forest()`](https://glycoverse.github.io/glyclean/reference/impute_miss_forest.md):
    MissForest imputation

- info:

  Internal parameter used by
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md).

## Value

The imputed experiment.

## Details

By default, all imputation methods are included for benchmarking when QC
samples are available. Note that some methods (e.g., MissForest) may be
slow for large datasets.

## Examples

``` r
library(glyexp)
exp_imputed <- auto_impute(real_experiment)
#> ℹ No QC samples found. Using default imputation method based on sample size.
#> ℹ Sample size <= 30, using `impute_sample_min()`.
```
