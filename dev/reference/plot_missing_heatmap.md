# Plot Missing Value Heatmap

Draw a binary heatmap showing the missing value pattern in an
experiment. Values are binarized: 1 (present) or 0 (missing). Rows
(variables) are sorted by missing value proportion from low to high.
Columns (samples) are clustered using hierarchical clustering.

## Usage

``` r
plot_missing_heatmap(exp, ...)
```

## Arguments

- exp:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- ...:

  Other arguments passed to
  [`pheatmap::pheatmap()`](https://rdrr.io/pkg/pheatmap/man/pheatmap.html).

## Value

A ggplot object of the missing value heatmap.

## Examples

``` r
plot_missing_heatmap(glyexp::real_experiment)

```
