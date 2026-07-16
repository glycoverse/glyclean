# Detect batch effect

Use ANOVA to detect if batch effect is present in the data. If `group`
is provided, it will be used as a covariate in the ANOVA model.

## Usage

``` r
detect_batch_effect(x, batch = "batch", group = NULL)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- batch:

  Either a factor/character vector specifying batch assignments for each
  sample, or a string specifying the column name in `sample_info`.
  Defaults to "batch".

- group:

  Either a factor/character vector specifying group assignments for each
  sample, or a string specifying the column name in `sample_info`. If
  provided, it will be used as a covariate in the ANOVA model. This is
  useful when you have an unbalanced design. Default to NULL.

## Value

A double vector of p-values for each variable, with the same length as
`nrow(x)`.

## Examples

``` r
library(SummarizedExperiment)

# With a SummarizedExperiment and column names
exp <- glyexp::real_experiment
batch <- rep(c("A", "B"), length.out = ncol(exp))
group <- rep(c("Ctrl", "Ctrl", "Treat", "Treat"), length.out = ncol(exp))
colData(exp)$batch <- batch
colData(exp)$group <- group
p_values <- detect_batch_effect(exp, batch = "batch", group = "group")
#> ℹ Detecting batch effects using ANOVA for 4262 variables...
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> Warning: ANOVA F-tests on an essentially perfect fit are unreliable
#> ✔ Batch effect detection completed. 65 out of 4262 variables show significant batch effects (p < 0.05).
```
