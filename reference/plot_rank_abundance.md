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
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

## Value

A ggplot object of protein rank abundance.

## Examples

``` r
plot_rank_abundance(glyexp::real_experiment)

```
