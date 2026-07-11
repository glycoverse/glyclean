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
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

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
# With glyexp_experiment and column names
exp <- glyexp::toy_experiment
exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
exp$sample_info$group <- c("Ctrl", "Ctrl", "Treat", "Ctrl", "Treat", "Treat")
p_values <- detect_batch_effect(exp, batch = "batch", group = "group")
#> ℹ Detecting batch effects using ANOVA for 4 variables...
#> ✔ Batch effect detection completed. 4 out of 4 variables show significant batch effects (p < 0.05).
```
