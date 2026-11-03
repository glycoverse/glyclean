# Reproducible single-assay pipelines

## Discover the pipeline after preprocessing

A pipeline records the preprocessing steps applied to an experiment.
Start by normalizing and imputing the bundled glycoproteomics experiment
with ordinary function calls:

``` r

library(glyclean)
#> 
#> Attaching package: 'glyclean'
#> The following object is masked from 'package:stats':
#> 
#>     aggregate

original <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
#> Warning: replacing previous import 'S4Arrays::makeNindexFromArrayViewport' by
#> 'DelayedArray::makeNindexFromArrayViewport' when loading 'SummarizedExperiment'
cleaned <- normalize_total_area(original)
cleaned <- impute_min_prob(cleaned, q = 0.01, seed = 42)
```

These calls automatically add a pipeline to the experiment’s metadata.
Use
[`get_pipeline()`](https://glycoverse.github.io/glyclean/dev/reference/glyclean_pipeline.md)
to retrieve it and print the recorded steps:

``` r

history <- get_pipeline(cleaned)
history
#> ── <glyclean_pipeline> 2 steps ─────────────────────────────────────────────────
#>   1. normalize_total_area
#>      (no parameters)
#>   2. impute_min_prob
#>     by = NULL, q = 0.01, tune.sigma = 1, seed = 42

identical(history, S4Vectors::metadata(cleaned)$pipeline)
#> [1] TRUE
```

A `glyclean_pipeline` is a list of steps. Each step contains a
`function` and its `parameters`, in the order they were applied. The
printed summary shows the function names and their parameters.

Automatic functions record the actual functions chosen. For example,
[`auto_normalize()`](https://glycoverse.github.io/glyclean/dev/reference/auto_normalize.md)
selects median normalization for this glycoproteomics experiment, so its
history contains
[`normalize_median()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_median.md):

``` r

automatically_normalized <- auto_normalize(original)
#> ℹ Normalization method: `normalize_median()`
#> ℹ Reason: default for "glycoproteomics".
get_pipeline(automatically_normalized)
#> ── <glyclean_pipeline> 1 step ──────────────────────────────────────────────────
#>   1. normalize_median
#>      (no parameters)
```

## Reproduce preprocessing from its history

Apply the recorded history to the same original experiment to reproduce
the preprocessing exactly:

``` r

replayed <- apply_pipeline(original, history)
identical(replayed, cleaned)
#> [1] TRUE
stopifnot(identical(replayed, cleaned))
```

This compares the whole experiment, including its assay, annotations and
pipeline metadata. Exact replay assumes the same original input, package
versions and random-number generator configuration; seeds are recorded
in the step parameters.

## Create a pipeline manually

When you already know the steps you want, define a recipe with
[`glyclean_pipeline()`](https://glycoverse.github.io/glyclean/dev/reference/glyclean_pipeline.md).
Omit the experiment argument and use full argument names for any
parameters:

``` r

recipe <- glyclean_pipeline(
  normalize_total_area(),
  impute_min_prob(q = 0.01, seed = 42)
)
recipe
#> ── <glyclean_pipeline> 2 steps ─────────────────────────────────────────────────
#>   1. normalize_total_area
#>      (no parameters)
#>   2. impute_min_prob
#>     q = 0.01, seed = 42, by = NULL, tune.sigma = 1
```

Building the recipe does not preprocess any data. Supply the experiment
when you apply it:

``` r

from_recipe <- apply_pipeline(original, recipe)
identical(from_recipe, cleaned)
#> [1] TRUE
stopifnot(identical(from_recipe, cleaned))
```

Recipes can also include `auto_*()` calls to choose methods when they
are applied. The resulting experiment’s history records the concrete
operations that ran, ready to inspect or replay.
