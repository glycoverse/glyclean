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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
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
plot_int_boxplot(glyexp::toy_experiment)

plot_int_boxplot(glyexp::toy_experiment, by = "group")

```
