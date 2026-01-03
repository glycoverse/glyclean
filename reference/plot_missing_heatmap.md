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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

- ...:

  Other arguments passed to
  [`pheatmap::pheatmap()`](https://rdrr.io/pkg/pheatmap/man/pheatmap.html).

## Value

A ggplot object of the missing value heatmap.

## Examples

``` r
plot_missing_heatmap(glyexp::toy_experiment)

```
