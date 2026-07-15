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
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
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
`nrow(glyexp::get_expr_mat(x))`.

## Examples

``` r
library(SummarizedExperiment)

# With a SummarizedExperiment and column names
exp <- glyexp::real_experiment
batch <- rep(c("A", "B"), length.out = ncol(exp))
group <- rep(c("Ctrl", "Ctrl", "Treat", "Treat"), length.out = ncol(exp))
if (inherits(exp, "glyexp_experiment")) {
  exp$sample_info$batch <- batch
  exp$sample_info$group <- group
} else {
  SummarizedExperiment::colData(exp)$batch <- batch
  SummarizedExperiment::colData(exp)$group <- group
}
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
