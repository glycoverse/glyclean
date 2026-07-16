# Plot Total Intensity by Sample

Draw a bar plot showing total intensity (TIC) for each sample. Samples
are ordered from high to low TIC from left to right.

## Usage

``` r
plot_tic_bar(exp)
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

A ggplot object of total intensity by sample.

## Examples

``` r
plot_tic_bar(glyexp::real_experiment)

```
