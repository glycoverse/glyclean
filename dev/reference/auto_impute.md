# Automatic Imputation

This function automatically selects and applies a deterministic default
imputation method by sample count and experiment type. Quality Control
(QC) samples are inspected for workflow consistency, but they are not
used to benchmark or select the imputation method.

## Usage

``` r
auto_impute(exp, group_col = "group")
```

## Arguments

- exp:

  An
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html).

- group_col:

  The column name in sample_info for groups. Default is "group". Can be
  NULL when no group information is available.

## Value

The imputed experiment.

## Details

The automatic strategy uses these defaults:

- `n_samples < 30`:
  [`impute_min_prob()`](https://glycoverse.github.io/glyclean/dev/reference/impute_min_prob.md)
  for glycomics and glycoproteomics.

- `30 <= n_samples <= 100`:
  [`impute_bpca()`](https://glycoverse.github.io/glyclean/dev/reference/impute_bpca.md)
  for glycomics and
  [`impute_min_prob()`](https://glycoverse.github.io/glyclean/dev/reference/impute_min_prob.md)
  for glycoproteomics.

- `n_samples > 100`:
  [`impute_miss_forest()`](https://glycoverse.github.io/glyclean/dev/reference/impute_miss_forest.md)
  for glycomics and
  [`impute_bpca()`](https://glycoverse.github.io/glyclean/dev/reference/impute_bpca.md)
  for glycoproteomics.

Other experiment types use the glycoproteomics defaults as a
conservative fallback.
[`impute_sample_min()`](https://glycoverse.github.io/glyclean/dev/reference/impute_sample_min.md)
and
[`impute_half_sample_min()`](https://glycoverse.github.io/glyclean/dev/reference/impute_half_sample_min.md)
remain available for manual use, but they are not selected
automatically.

## Examples

``` r
library(glyexp)
exp_imputed <- auto_impute(real_experiment)
#> ℹ Imputation method: `impute_min_prob()`
#> ℹ Reason: default for "glycoproteomics" with n_samples < 30.
```
