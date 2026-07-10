# Median Quotient Normalization

This approach is based on the calculation of the dilution factor of each
sample with respect to a reference sample. Here, the reference sample
was calculated as the median value of each glycan's abundance across all
measured samples. For each sample, a vector of quotients was then
obtained by dividing each glycan measure by the corresponding value in
the reference sample. The median of these quotients was then used as the
sample's dilution factor, and the original sample values were
subsequently divided by that value. The underlying assumption is that
the diﬀerent intensities observed across individuals are imputable to
diﬀerent amounts of the biological material in the collected samples.
See [this paper](https://dx.doi.org/10.1021/ac051632c) for more
information.

## Usage

``` r
normalize_median_quotient(x, by = NULL)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix.
