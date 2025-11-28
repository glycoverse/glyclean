# Remove Constant Variables

Constant variables are variables with the same value in all samples.
This function is equivalent to
`remove_low_var(x, var_cutoff = 0, by = by, strict = strict)`.

## Usage

``` r
remove_constant(x, by = NULL, strict = FALSE)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix.

- by:

  Either a column name in `sample_info` (string) or a vector specifying
  group assignments for each sample.

- strict:

  If `FALSE`, remove a variable only if it is constant in all groups. If
  `TRUE`, remove a variable if it is constant in any group. Defaults to
  FALSE.

## Value

For `glyexp_experiment` input, returns a modified `glyexp_experiment`
object. For matrix input, returns a filtered matrix.

## See also

[`remove_low_var()`](https://glycoverse.github.io/glyclean/reference/remove_low_var.md)
