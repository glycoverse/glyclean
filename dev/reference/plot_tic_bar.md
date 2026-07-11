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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

## Value

A ggplot object of total intensity by sample.

## Examples

``` r
plot_tic_bar(glyexp::toy_experiment)

```
