# Plot Log-Intensity Boxplots by Sample

Draw boxplots of log2-transformed intensities for each sample.
Optionally color and group samples by a metadata variable.

## Usage

``` r
plot_int_boxplot(exp, by = NULL)
```

## Arguments

- exp:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- by:

  Grouping variable for samples. Can be a column name in `sample_info`
  or a vector/factor with length equal to the number of samples. When
  provided, samples are grouped along the x-axis and boxplots are
  colored by group.

## Value

A ggplot object of log-intensity boxplots.

## Examples

``` r
plot_int_boxplot(glyexp::real_experiment)
#> Warning: Removed 4063 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).

plot_int_boxplot(glyexp::real_experiment, by = "group")
#> Warning: Removed 4063 rows containing non-finite outside the scale range
#> (`stat_boxplot()`).

```
