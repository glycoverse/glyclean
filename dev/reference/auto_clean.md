# Automatic Data Preprocessing

Perform automatic data preprocessing on glycoproteomics or glycomics
data. This function applies an intelligent preprocessing pipeline that
includes normalization, missing value handling, imputation, aggregation
(for glycoproteomics data), and batch effect correction.

For glycomics data, this function calls these functions in sequence:

- [`auto_remove()`](https://glycoverse.github.io/glyclean/dev/reference/auto_remove.md)

- [`auto_impute()`](https://glycoverse.github.io/glyclean/dev/reference/auto_impute.md)

- [`auto_normalize()`](https://glycoverse.github.io/glyclean/dev/reference/auto_normalize.md)

- [`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/dev/reference/auto_correct_batch_effect.md)

For glycoproteomics data, this function calls these functions in
sequence:

- [`auto_normalize()`](https://glycoverse.github.io/glyclean/dev/reference/auto_normalize.md)

- [`auto_remove()`](https://glycoverse.github.io/glyclean/dev/reference/auto_remove.md)

- [`auto_impute()`](https://glycoverse.github.io/glyclean/dev/reference/auto_impute.md)

- [`auto_aggregate()`](https://glycoverse.github.io/glyclean/dev/reference/auto_aggregate.md)

- [`auto_normalize()`](https://glycoverse.github.io/glyclean/dev/reference/auto_normalize.md)

- [`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/dev/reference/auto_correct_batch_effect.md)

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
  batch_confounding_threshold = 0.4,
  standardize_variable = TRUE
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

  **\[deprecated\]** This function no longer uses QC sample information.
  This parameter is ignored and will be removed in a future release.

- normalize_to_try:

  **\[deprecated\]** This parameter is no longer used and will be
  removed in a future release. The automatic normalization strategy is
  now deterministic and does not require user-specified methods to try.

- impute_to_try:

  **\[deprecated\]** This parameter is no longer used and will be
  removed in a future release.

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

- standardize_variable:

  Whether to call
  [`glyexp::standardize_variable()`](https://glycoverse.github.io/glyexp/reference/standardize_variable.html)
  after aggregation. Set to `FALSE` to skip network calls for faster
  testing. Default is `TRUE`.

## Value

A modified
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object.

## See also

[`auto_normalize()`](https://glycoverse.github.io/glyclean/dev/reference/auto_normalize.md),
[`auto_remove()`](https://glycoverse.github.io/glyclean/dev/reference/auto_remove.md),
[`auto_impute()`](https://glycoverse.github.io/glyclean/dev/reference/auto_impute.md),
[`auto_aggregate()`](https://glycoverse.github.io/glyclean/dev/reference/auto_aggregate.md),
[`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/dev/reference/auto_correct_batch_effect.md)

## Examples

``` r
library(glyexp)
exp <- real_experiment
auto_clean(exp)
#> 
#> ── Normalizing data ──
#> 
#> ℹ Normalization method: `normalize_median()`
#> ℹ Reason: default for "glycoproteomics".
#> ✔ Normalization completed.
#> 
#> ── Removing variables with too many missing values ──
#> 
#> ℹ Applying preset "discovery"...
#> ℹ Total removed: 24 (0.56%) variables.
#> ✔ Variable removal completed.
#> 
#> ── Imputing missing values ──
#> 
#> ℹ Imputation method: `impute_min_prob()`
#> ℹ Reason: default for "glycoproteomics" with n_samples < 30.
#> ✔ Imputation completed.
#> 
#> ── Aggregating data ──
#> 
#> ℹ Aggregating to "gfs" level
#> ✔ Aggregation completed.
#> 
#> ── Normalizing data again ──
#> 
#> ℹ Normalization method: `normalize_median()`
#> ℹ Reason: default for "glycoproteomics".
#> ✔ Normalization completed.
#> 
#> ── Correcting batch effects ──
#> 
#> ℹ Batch column batch not found in sample_info. Skipping batch correction.
#> ✔ Batch correction completed.
#> 
#> ── Glycoproteomics Experiment ──────────────────────────────────────────────────
#> ℹ Expression matrix: 12 samples, 3979 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: protein <chr>, glycan_composition <comp>, glycan_structure <struct>, protein_site <int>, gene <chr>
```
