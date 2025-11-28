# Correct Batch Effect

Correct batch effects in glycoproteomics/glycomics data using ComBat
algorithm from the sva package.

## Usage

``` r
correct_batch_effect(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4
)

# S3 method for class 'glyexp_experiment'
correct_batch_effect(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4
)

# S3 method for class 'matrix'
correct_batch_effect(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4
)

# Default S3 method
correct_batch_effect(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4
)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- batch:

  Either a factor/character vector specifying batch assignments for each
  sample, or a string specifying the column name in sample_info (for
  experiment input only). Default to "batch" for experiment input.

- group:

  Either a factor/character vector specifying group assignments for each
  sample, or a string specifying the column name in sample_info (for
  experiment input only). If provided, it will be used as a covariate in
  the ComBat model. This is useful when you have an unbalanced design.
  Default to NULL.

- check_confounding:

  Whether to check for confounding between batch and group variables.
  Default to TRUE.

- confounding_threshold:

  The threshold for Cramer's V to consider batch and group variables
  highly confounded. Only used when `check_confounding` is TRUE. Default
  to 0.4.

## Value

For `glyexp_experiment` input, returns a modified `glyexp_experiment`
object. For matrix input, returns a batch-corrected matrix.

## Details

This function performs batch effect correction using the ComBat
algorithm. It requires batch information provided via the `batch`
parameter. If no batch information is available, the function will
return the original data unchanged.

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

# With matrix and factor vectors
mat <- matrix(abs(rnorm(200)), nrow = 20, ncol = 10)
batch_factor <- factor(rep(c("A", "B"), each = 5))
group_factor <- factor(rep(c("Ctrl", "Treat"), times = 5))
corrected_mat <- correct_batch_effect(mat, batch = batch_factor, group = group_factor)
#> Found2batches
#> Adjusting for1covariate(s) or covariate level(s)
#> Standardizing Data across genes
#> Fitting L/S model and finding priors
#> Finding parametric adjustments
#> Adjusting the Data
```
