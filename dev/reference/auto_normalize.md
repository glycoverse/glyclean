# Automatic Normalization

This function automatically applies a deterministic default
normalization method based on experiment type. Quality Control (QC)
samples are not used to benchmark, select, or report normalization
behavior.

## Usage

``` r
auto_normalize(exp, group_col = "group")
```

## Arguments

- exp:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- group_col:

  The column name in sample_info for groups. Default is "group". Can be
  NULL when no group information is available.

## Value

The normalized input container, with its class preserved.

## Details

The automatic strategy uses these defaults:

- `glycomics`:
  [`normalize_total_area()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_total_area.md).

- `glycoproteomics`:
  [`normalize_median()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_median.md).

- missing or other experiment types:
  [`normalize_median()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_median.md).

## Examples

``` r
library(glyexp)
exp_normed <- auto_normalize(real_experiment)
#> ℹ Normalization method: `normalize_median()`
#> ℹ Reason: default for "glycoproteomics".
```
