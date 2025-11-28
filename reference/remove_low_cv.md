# Remove Variables with Low Coefficient of Variation

Filters variables whose coefficient of variation falls below a
threshold. Default behavior is to remove variables with zero coefficient
of variation.

## Usage

``` r
remove_low_cv(x, cv_cutoff = 0, by = NULL, strict = FALSE)

# S3 method for class 'glyexp_experiment'
remove_low_cv(x, cv_cutoff = 0, by = NULL, strict = FALSE)

# S3 method for class 'matrix'
remove_low_cv(x, cv_cutoff = 0, by = NULL, strict = FALSE)

# Default S3 method
remove_low_cv(x, cv_cutoff = 0, by = NULL, strict = FALSE)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix.

- cv_cutoff:

  The cutoff for coefficient of variation. Defaults to 0.

- by:

  A factor specifying the groupings. Defaults to NULL.

- strict:

  If `FALSE`, remove a variable only if it passes the coefficient of
  variation threshold in all groups. If `TRUE`, remove a variable if it
  passes the coefficient of variation threshold in any group.

## Value

For `glyexp_experiment` input, returns a modified `glyexp_experiment`
object. For matrix input, returns a filtered matrix.

## See also

[`remove_low_var()`](https://glycoverse.github.io/glyclean/reference/remove_low_var.md)
