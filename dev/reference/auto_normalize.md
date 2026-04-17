# Automatic Normalization

This function automatically selects and applies the most suitable
normalization method for the given dataset. If Quality Control (QC)
samples are present, the method that best stabilizes them (i.e., yields
the lowest median coefficient of variation) is chosen. Otherwise, it
defaults to median normalization for glycoproteomics data, and total
area normalization for glycomics data.

## Usage

``` r
auto_normalize(
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

  Normalization functions to try when QC samples are present. A list.
  Default includes:

  - [`normalize_median()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_median.md):
    median normalization

  - [`normalize_median_abs()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_median_abs.md):
    absolute median normalization

  - [`normalize_total_area()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_total_area.md):
    total area normalization

  - [`normalize_quantile()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_quantile.md):
    quantile normalization

  - [`normalize_loessf()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_loessf.md):
    LoessF normalization

  - [`normalize_loesscyc()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_loesscyc.md):
    LoessCyc normalization

  - [`normalize_median_quotient()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_median_quotient.md):
    median quotient normalization

  - [`normalize_rlr()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_rlr.md):
    Robust Linear Regression normalization

  - [`normalize_rlrma()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_rlrma.md):
    Robust Linear Regression with Median Adjustment normalization

  - [`normalize_rlrmacyc()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_rlrmacyc.md):
    Robust Linear Regression with Median Adjustment and Cyclic
    normalization

- info:

  Internal parameter used by
  [`auto_clean()`](https://glycoverse.github.io/glyclean/dev/reference/auto_clean.md).

## Value

The normalized experiment.

## Details

When QC samples are available, the function benchmarks all normalization
methods in `to_try` and selects the one with the lowest median
coefficient of variation (CV) among QC samples. Methods that fail are
skipped with a warning.

## Examples

``` r
library(glyexp)
exp_normed <- auto_normalize(real_experiment)
#> ℹ No QC samples found. Using default normalization method based on experiment type.
#> ℹ Experiment type is "glycoproteomics". Using `normalize_median()`.
```
