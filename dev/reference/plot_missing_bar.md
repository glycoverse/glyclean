# Plot Missing Value Proportions by Sample or Variable

Draw a bar plot of missing value proportions for each sample or
variable. Items are ordered from low to high missing proportion.

## Usage

``` r
plot_missing_bar(exp, on = "sample")
```

## Arguments

- exp:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- on:

  Whether to plot missingness by `"sample(s)"` or `"variable(s)"`.
  Defaults to `"sample"`.

## Value

A ggplot object of missing value proportions by item.

## See also

[data_scales](https://glycoverse.github.io/glyclean/dev/reference/data_scales.md)
for working domains and supported input scales.

## Examples

``` r
plot_missing_bar(glyexp::real_experiment)

```
