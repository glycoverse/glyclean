# Automatic Removing Variables

This function uses preset rules to remove variables with low quality.
Available presets:

- "simple": remove variables with more than 50% missing values.

- "discovery": more lenient, remove variables with more than 80% missing
  values, but ensure less than 50% of missing values in at least one
  group.

- "biomarker": more strict, remove variables with more than 40% missing
  values, and ensure less than 60% of missing values in all groups.

## Usage

``` r
auto_remove(exp, preset = "discovery", group_col = "group")
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- preset:

  One of "simple", "discovery", or "biomarker". Default "discovery" if
  group information is available, otherwise "simple".

- group_col:

  The column name in sample_info for groups. Default is "group". Can be
  NULL when no group information is available.

## Value

The filtered input container. SummarizedExperiment inputs return the
same class.

## Examples

``` r
library(glyexp)
exp <- real_experiment
auto_remove(exp)
#> ℹ Applying preset "discovery"...
#> ℹ Total removed: 24 (0.56%) variables.
#> 
#> ── Glycoproteomics Experiment ──────────────────────────────────────────────────
#> ℹ Expression matrix: 12 samples, 4238 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: peptide <chr>, peptide_site <int>, protein <chr>, protein_site <int>, gene <chr>, glycan_composition <comp>, glycan_structure <struct>
```
