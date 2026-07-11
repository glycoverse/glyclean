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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

- by:

  Grouping variable for samples. Can be a column name in `sample_info`
  or a vector/factor with length equal to the number of samples. When
  provided, CVs are computed within each group and densities are shown
  with different fills.

## Value

A ggplot object of CV density.

## Examples

``` r
plot_cv_dent(glyexp::toy_experiment)

exp <- glyexp::toy_experiment
exp$sample_info$group <- rep(c("A", "B"), length.out = ncol(exp$expr_mat))
plot_cv_dent(exp, by = "group")

```
