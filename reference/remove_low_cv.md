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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
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

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with filtered variables. SummarizedExperiment inputs return the
same class.

## See also

[`remove_low_var()`](https://glycoverse.github.io/glyclean/reference/remove_low_var.md)
