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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

- on:

  Whether to plot missingness by `"sample(s)"` or `"variable(s)"`.
  Defaults to `"sample"`.

## Value

A ggplot object of missing value proportions by item.

## Examples

``` r
plot_missing_bar(glyexp::toy_experiment)

```
