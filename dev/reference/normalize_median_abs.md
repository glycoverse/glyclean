# Absolute Median Normalization

Normalize the expression matrix by dividing each column (sample) by the
absolute median of that column (NA ignored), so that the absolute median
of each column is 1.

## Usage

``` r
normalize_median_abs(x)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix.
