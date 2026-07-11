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

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- by:

  Either a column name in `sample_info` (string) or a vector specifying
  group assignments for each sample.

- strict:

  If `FALSE`, remove a variable only if it is constant in all groups. If
  `TRUE`, remove a variable if it is constant in any group. Defaults to
  FALSE.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with filtered variables. SummarizedExperiment inputs return the
same class.

## See also

[`remove_low_var()`](https://glycoverse.github.io/glyclean/dev/reference/remove_low_var.md)
