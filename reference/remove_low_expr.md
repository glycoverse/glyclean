# Remove Variables with Low Expression

Filters variables based on median expression values. Variables with
median expression values below certain percentile will be removed.

## Usage

``` r
remove_low_expr(x, percentile = 0.05, by = NULL, strict = FALSE)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- percentile:

  The percentile for median expression values. Defaults to 0.05, i.e.,
  the 5% lowest median expression values will be removed.

- by:

  Either a column name in `sample_info` (string) or a vector specifying
  group assignments for each sample.

- strict:

  If `FALSE`, remove a variable only if it passes the abundance
  thresholds in all groups. If `TRUE`, remove a variable if it passes
  the abundance thresholds in any group. Defaults to FALSE.

## Value

A container of the same class as `x`, with filtered variables.
