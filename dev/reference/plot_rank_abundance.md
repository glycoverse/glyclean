# Plot Rank Abundance

Draw a scatter plot of proteins ranked by mean log2 intensity. Proteins
are ordered from high to low mean intensity along the x-axis.

## Usage

``` r
plot_rank_abundance(exp)
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

## Value

A ggplot object of protein rank abundance.

## Examples

``` r
plot_rank_abundance(glyexp::toy_experiment)

```
