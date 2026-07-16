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
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- batch_col:

  Column name in `sample_info`, or a factor/vector with length equal to
  the number of samples.

## Value

A ggplot object of PCA scores.

## Examples

``` r
library(SummarizedExperiment)

exp <- glyexp::real_experiment
batch <- rep(c("A", "B"), length.out = ncol(exp))
colData(exp)$batch <- batch
plot_batch_pca(exp, batch_col = "batch")

```
