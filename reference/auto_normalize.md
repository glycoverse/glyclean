# Automatic Normalization

This function automatically applies a deterministic default
normalization method based on experiment type. Quality Control (QC)
samples are not used to benchmark, select, or report normalization
behavior.

## Usage

``` r
auto_normalize(exp, group_col = "group", qc_name = "QC", to_try = NULL)
```

## Arguments

- exp:

  An
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html).

- group_col:

  The column name in sample_info for groups. Default is "group". Can be
  NULL when no group information is available.

- qc_name:

  **\[deprecated\]** This function no longer uses this argument. This
  parameter is ignored and will be removed in a future release.

- to_try:

  **\[deprecated\]** This parameter is no longer used and will be
  removed in a future release. The automatic strategy is now
  deterministic and does not require user-specified methods to try.

## Value

The normalized experiment.

## Details

The automatic strategy uses these defaults:

- `glycomics`:
  [`normalize_total_area()`](https://glycoverse.github.io/glyclean/reference/normalize_total_area.md).

- `glycoproteomics`:
  [`normalize_median()`](https://glycoverse.github.io/glyclean/reference/normalize_median.md).

- missing or other experiment types:
  [`normalize_median()`](https://glycoverse.github.io/glyclean/reference/normalize_median.md).

## Examples

``` r
library(glyexp)
exp_normed <- auto_normalize(real_experiment)
#> ℹ Normalization method: `normalize_median()`
#> ℹ Reason: default for "glycoproteomics".
```
