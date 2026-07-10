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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
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

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with filtered variables.

## See also

[`remove_low_cv()`](https://glycoverse.github.io/glyclean/dev/reference/remove_low_cv.md),
[`remove_constant()`](https://glycoverse.github.io/glyclean/dev/reference/remove_constant.md)
