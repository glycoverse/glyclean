# Correct Batch Effect

Correct batch effects in glycoproteomics/glycomics data using ComBat
algorithm from the sva package or removeBatchEffect from the limma
package.

## Usage

``` r
correct_batch_effect(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  method = c("combat", "limma")
)
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
  provided, it will be used as a covariate in the batch correction
  model. This is useful when you have an unbalanced design. Default to
  NULL.

- check_confounding:

  Whether to check for confounding between batch and group variables.
  Default to TRUE.

- confounding_threshold:

  The threshold for Cramer's V to consider batch and group variables
  highly confounded. Only used when `check_confounding` is TRUE. Default
  to 0.4.

- method:

  The batch correction method to use. Either "combat" (default, uses
  sva::ComBat) or "limma" (uses limma::removeBatchEffect). Default to
  "combat".

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with batch effects corrected. Glyco SE inputs return the same
subclass.

## Details

This function performs batch effect correction using either the ComBat
algorithm or the limma removeBatchEffect function. It requires batch
information provided via the `batch` parameter. If no batch information
is available, the function will return the original data unchanged.

If group information is provided via `group`, the function will check
for confounding between batch and group variables. If batch and group
are highly confounded (\|Cramer's V\| \> `threshold`), the function will
issue a warning and return the original data unchanged to avoid
over-correction.

When both batch and group information are available and not highly
confounded, the group information will be included in the model to
preserve biological variation while correcting for batch effects.

## Examples

``` r
# With glyexp_experiment and column names
exp <- glyexp::toy_experiment
exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
exp$sample_info$group <- c("Ctrl", "Ctrl", "Treat", "Ctrl", "Treat", "Treat")
corrected_exp <- correct_batch_effect(exp, batch = "batch", group = "group")
#> Found2batches
#> Adjusting for1covariate(s) or covariate level(s)
#> Standardizing Data across genes
#> Fitting L/S model and finding priors
#> Finding parametric adjustments
#> Adjusting the Data

# Using limma method
corrected_exp <- correct_batch_effect(
  exp, batch = "batch", group = "group", method = "limma"
)
#> design matrix of interest not specified. Assuming a one-group experiment.
#> Coefficients not estimable: (Intercept) 
```
