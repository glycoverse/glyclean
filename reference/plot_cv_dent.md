# Plot CV Density

Compute coefficient of variation (CV) for each variable and plot its
density. When `by` is provided, CVs are computed within each group and
densities are shown with different fills.

## Usage

``` r
plot_cv_dent(exp, by = NULL)
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
  provided, CVs are computed within each group and densities are shown
  with different fills.

## Value

A ggplot object of CV density.

## Examples

``` r
plot_cv_dent(glyexp::real_experiment)

library(SummarizedExperiment)

exp <- glyexp::real_experiment
group <- rep(c("A", "B"), length.out = ncol(exp))
colData(exp)$group <- group
plot_cv_dent(exp, by = "group")

```
