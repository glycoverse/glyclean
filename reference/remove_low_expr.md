# Remove Variables with Low Expression

Filters variables based on median expression values. Variables with
median expression values below certain percentile will be removed.

## Usage

``` r
remove_low_expr(x, percentile = 0.05, by = NULL, strict = FALSE)

# S3 method for class 'glyexp_experiment'
remove_low_expr(x, percentile = 0.05, by = NULL, strict = FALSE)

# S3 method for class 'matrix'
remove_low_expr(x, percentile = 0.05, by = NULL, strict = FALSE)

# Default S3 method
remove_low_expr(x, percentile = 0.05, by = NULL, strict = FALSE)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix.

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

For `glyexp_experiment` input, returns a modified `glyexp_experiment`
object. For matrix input, returns a filtered matrix.
