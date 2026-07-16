# Remove Variables with Low Variance Remove Variables with Low Variance

Filters variables whose variance falls below a threshold. Default
behavior is to remove variables with zero variance.

## Usage

``` r
remove_low_var(x, var_cutoff = 0, by = NULL, strict = FALSE)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- var_cutoff:

  The cutoff for variance. Defaults to 0.

- by:

  A factor specifying the groupings. Defaults to NULL.

- strict:

  If `FALSE`, remove a variable only if it passes the variance threshold
  in all groups. If `TRUE`, remove a variable if it passes the variance
  threshold in any group.

## Value

A container of the same class as `x`, with filtered variables.

## See also

[`remove_low_cv()`](https://glycoverse.github.io/glyclean/dev/reference/remove_low_cv.md),
[`remove_constant()`](https://glycoverse.github.io/glyclean/dev/reference/remove_constant.md)
