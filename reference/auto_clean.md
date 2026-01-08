# Automatic Data Preprocessing

Perform automatic data preprocessing on glycoproteomics or glycomics
data. This function applies an intelligent preprocessing pipeline that
includes normalization, missing value handling, imputation, aggregation
(for glycoproteomics data), and batch effect correction.

For glycomics data, this function calls these functions in sequence:

- [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md)

- [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)

- [`normalize_total_area()`](https://glycoverse.github.io/glyclean/reference/normalize_total_area.md)

- [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)

- [`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/auto_correct_batch_effect.md)

For glycoproteomics data, this function calls these functions in
sequence:

- [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)

- [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md)

- [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)

- [`auto_aggregate()`](https://glycoverse.github.io/glyclean/reference/auto_aggregate.md)

- [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)

- [`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/auto_correct_batch_effect.md)

## Usage

``` r
auto_clean(
  exp,
  group_col = "group",
  batch_col = "batch",
  qc_name = "QC",
  normalize_to_try = NULL,
  impute_to_try = NULL,
  remove_preset = "discovery",
  batch_prop_threshold = 0.3,
  check_batch_confounding = TRUE,
  batch_confounding_threshold = 0.4
)
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  containing glycoproteomics or glycomics data.

- group_col:

  The column name in sample_info for groups. Default is "group". Can be
  NULL when no group information is available.

- batch_col:

  The column name in sample_info for batches. Default is "batch". Can be
  NULL when no batch information is available.

- qc_name:

  The name of QC samples in the `group_col` column. Default is "QC".
  Only used when `group_col` is not NULL. Can be NULL when no QC samples
  are available.

- normalize_to_try:

  Normalization functions to try. A list. Default includes:

  - [`normalize_median()`](https://glycoverse.github.io/glyclean/reference/normalize_median.md):
    median normalization

  - [`normalize_median_abs()`](https://glycoverse.github.io/glyclean/reference/normalize_median_abs.md):
    absolute median normalization

  - [`normalize_total_area()`](https://glycoverse.github.io/glyclean/reference/normalize_total_area.md):
    total area mormalization

  - [`normalize_quantile()`](https://glycoverse.github.io/glyclean/reference/normalize_quantile.md):
    quantile normalization

  - [`normalize_loessf()`](https://glycoverse.github.io/glyclean/reference/normalize_loessf.md):
    LoessF normalization

  - [`normalize_loesscyc()`](https://glycoverse.github.io/glyclean/reference/normalize_loesscyc.md):
    LoessCyc normalization

  - [`normalize_rlr()`](https://glycoverse.github.io/glyclean/reference/normalize_rlr.md):
    RLR normalization

  - [`normalize_rlrma()`](https://glycoverse.github.io/glyclean/reference/normalize_rlrma.md):
    RLRMA normalization

  - [`normalize_rlrmacyc()`](https://glycoverse.github.io/glyclean/reference/normalize_rlrmacyc.md):
    RLRMAcyc normalization

- impute_to_try:

  Imputation functions to try. A list. Default includes:

  - [`impute_zero()`](https://glycoverse.github.io/glyclean/reference/impute_zero.md):
    zero imputation

  - [`impute_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_sample_min.md):
    sample-wise minimum imputation

  - [`impute_half_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_half_sample_min.md):
    half sample-wise minimum imputation

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

- remove_preset:

  The preset for removing variables. Default is "discovery". Available
  presets:

  - "simple": remove variables with more than 50% missing values.

  - "discovery": more lenient, remove variables with more than 80%
    missing values, but ensure less than 50% of missing values in at
    least one group.

  - "biomarker": more strict, remove variables with more than 40%
    missing values, and ensure less than 60% of missing values in all
    groups.

- batch_prop_threshold:

  The proportion of variables that must show significant batch effects
  to perform batch correction. Default is 0.3 (30%).

- check_batch_confounding:

  Whether to check for confounding between batch and group variables.
  Default to TRUE.

- batch_confounding_threshold:

  The threshold for Cramer's V to consider batch and group variables
  highly confounded. Only used when `check_batch_confounding` is TRUE.
  Default to 0.4.

## Value

A modified
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object.

## See also

[`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md),
[`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md),
[`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md),
[`auto_aggregate()`](https://glycoverse.github.io/glyclean/reference/auto_aggregate.md),
[`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/auto_correct_batch_effect.md)

## Examples

``` r
library(glyexp)
exp <- real_experiment
auto_clean(exp)
#> 
#> ── Normalizing data ──
#> 
#> ℹ No QC samples found. Using default normalization method based on experiment type.
#> ℹ Experiment type is "glycoproteomics". Using `normalize_median()`.
#> ✔ Normalization completed.
#> 
#> ── Removing variables with too many missing values ──
#> 
#> ℹ No QC samples found. Using all samples.
#> ℹ Applying preset "discovery"...
#> ℹ Total removed: 24 (0.56%) variables.
#> ✔ Variable removal completed.
#> 
#> ── Imputing missing values ──
#> 
#> ℹ No QC samples found. Using default imputation method based on sample size.
#> ℹ Sample size <= 30, using `impute_sample_min()`.
#> ✔ Imputation completed.
#> 
#> ── Aggregating data ──
#> 
#> ℹ Aggregating to "gfs" level
#> ✔ Aggregation completed.
#> 
#> ── Normalizing data again ──
#> 
#> ℹ No QC samples found. Using default normalization method based on experiment type.
#> ℹ Experiment type is "glycoproteomics". Using `normalize_median()`.
#> ✔ Normalization completed.
#> 
#> ── Correcting batch effects ──
#> 
#> ℹ Batch column  not found in sample_info. Skipping batch correction.
#> ✔ Batch correction completed.
#> 
#> ── Glycoproteomics Experiment ──────────────────────────────────────────────────
#> ℹ Expression matrix: 12 samples, 3979 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: protein <chr>, glycan_composition <glyrpr_c>, glycan_structure <glyrpr_s>, protein_site <int>, gene <chr>
```
