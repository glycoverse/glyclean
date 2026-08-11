# Automatic Aggregation

**\[deprecated\]**

`auto_aggregate()` was deprecated in glyclean 0.15.3. Use
[`aggregate()`](https://glycoverse.github.io/glyclean/dev/reference/aggregate.md)
instead; it now selects the aggregation level from the input type and
whether `glycan_structure` is present.

## Usage

``` r
auto_aggregate(exp, standardize_variable = TRUE)
```

## Arguments

- exp:

  A glycomics or glycoproteomics container: a
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or legacy `glyexp_experiment` object.

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
# Deprecated:
# auto_aggregate(exp)

# Use instead:
aggregate(exp)
#> 
#> ── GlycoproteomicSE ────────────────────────────────────────────────────────────
#> ℹ Abundance assay: 12 samples, 4001 variables
#> ℹ Glycan type: N
#> ℹ Row data fields: protein <chr>, glycan_composition <comp>, glycan_structure <struct>, protein_site <int>, gene <chr>
#> ℹ Column data fields: group <fct>
#> ℹ Metadata fields: exp_type <chr>, glycan_type <chr>, quant_method <chr>
```
