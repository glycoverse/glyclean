# Automatic Batch Correction

Detects and corrects batch effects in the experiment. If batch
information is available, this function performs ANOVA to detect batch
effects. If more than 30% (controlled by `prop_threshold`) of variables
show significant batch effects (p \< 0.05), batch correction is
performed using ComBat. If group information exists, it will be used as
a covariate in both detection and correction to preserve biological
variation. If no batch information is available, the function will
return the original experiment.

## Usage

``` r
auto_correct_batch_effect(
  exp,
  group_col = "group",
  batch_col = "batch",
  prop_threshold = 0.3,
  check_confounding = TRUE,
  confounding_threshold = 0.4
)
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

- batch_col:

  The column name in sample_info for batches. Default is "batch". Can be
  NULL when no batch information is available.

- prop_threshold:

  The proportion of variables that must show significant batch effects
  to perform batch correction. Default is 0.3 (30%).

- check_confounding:

  Whether to check for confounding between batch and group variables.
  Default to TRUE.

- confounding_threshold:

  The threshold for Cramer's V to consider batch and group variables
  highly confounded. Only used when `check_confounding` is TRUE. Default
  to 0.4.

## Value

The input container with batch effects corrected and its class
preserved.

## Examples

``` r
exp <- glyexp::real_experiment
exp <- auto_correct_batch_effect(exp)
#> ℹ Batch column batch not found in sample_info. Skipping batch correction.
```
