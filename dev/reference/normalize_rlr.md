# Robust Linear Regression Normalization

This method is based on robust linear regression. The reference sample
is calculated as the median value of each variable's abundance across
all measured samples. For each sample, a robust linear regression model
is fitted to the sample's abundance values against the reference
sample's abundance values. The fitted model is then used to normalize
the sample's abundance values. The underlying assumption is that the
diﬀerent intensities observed across individuals are imputable to
diﬀerent amounts of the biological material in the collected samples.

## Usage

``` r
normalize_rlr(x, by = NULL)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix. Glyco SE inputs return the
same subclass.
