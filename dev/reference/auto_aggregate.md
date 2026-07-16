# Automatic Aggregation

Aggregates glycoproteomics data to "gfs" (glycoforms with structures)
level if the glycan structure column exists, otherwise to "gf"
(glycoforms with compositions) level.

## Usage

``` r
auto_aggregate(exp, standardize_variable = TRUE)
```

## Arguments

- exp:

  A
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  object.

- standardize_variable:

  Whether to call
  [`glyexp::standardize_variable()`](https://glycoverse.github.io/glyexp/reference/standardize_variable.html)
  after aggregation. Set to `FALSE` to skip network calls for faster
  testing. Default is `TRUE`.

## Value

A modified container with the same class as `exp`, an aggregated
expression matrix, and updated variable information.

## Examples

``` r
library(glyexp)
exp <- real_experiment
auto_aggregate(exp)
#> ℹ Aggregating to "gfs" level
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4001 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: protein <chr>, glycan_composition <comp>, glycan_structure <struct>, protein_site <int>, gene <chr>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
