# Automatic Aggregation

Aggregates glycoproteomics data to "gfs" (glycoforms with structures)
level if the glycan structure column exists, otherwise to "gf"
(glycoforms with compositions) level.

## Usage

``` r
auto_aggregate(exp)
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object with "glycoproteomics" type.

## Value

A modified
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with aggregated expression matrix and updated variable
information.

## Examples

``` r
library(glyexp)
exp <- real_experiment
auto_aggregate(exp)
#> Aggregating to "gfs" level
#> 
#> ── Glycoproteomics Experiment ──────────────────────────────────────────────────
#> ℹ Expression matrix: 12 samples, 4001 variables
#> ℹ Sample information fields: group <fct>
#> ℹ Variable information fields: protein <chr>, glycan_composition <glyrpr_c>, glycan_structure <glyrpr_s>, protein_site <int>, gene <chr>
```
