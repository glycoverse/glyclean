# Remove Variables with Low Coefficient of Variation

Filters variables whose coefficient of variation falls below a
threshold. Default behavior is to remove variables with zero coefficient
of variation.

## Usage

``` r
remove_low_cv(x, cv_cutoff = 0, by = NULL, strict = FALSE)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- cv_cutoff:

  The cutoff for coefficient of variation. Defaults to 0.

- by:

  A factor specifying the groupings. Defaults to NULL.

- strict:

  If `FALSE`, remove a variable only if it passes the coefficient of
  variation threshold in all groups. If `TRUE`, remove a variable if it
  passes the coefficient of variation threshold in any group.

## Value

A container of the same class as `x`, with filtered variables.

## See also

[`remove_low_var()`](https://glycoverse.github.io/glyclean/reference/remove_low_var.md)
