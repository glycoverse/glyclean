# Plot PCA Score by Batch

Draw a PCA score plot for samples and color points by batch. PCA is
computed on log2-transformed intensities after removing variables with
missing values.

## Usage

``` r
plot_batch_pca(exp, batch_col = "batch")
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

- batch_col:

  Column name in `sample_info`, or a factor/vector with length equal to
  the number of samples.

## Value

A ggplot object of PCA scores.

## Examples

``` r
exp <- glyexp::toy_experiment
exp$sample_info$batch <- rep(c("A", "B"), each = 3)
plot_batch_pca(exp, batch_col = "batch")

```
