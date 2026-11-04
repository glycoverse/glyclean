# Reproducible preprocessing pipelines

Build an unevaluated sequence of glyclean processing calls, apply it to
one experiment, or retrieve the processing history of an experiment.

## Usage

``` r
glyclean_pipeline(...)

get_pipeline(exp)

apply_pipeline(exp, pipeline)
```

## Arguments

- ...:

  Calls to glyclean processing functions. Omit the experiment and name
  every argument explicitly; positional and partially matched arguments
  are not accepted. Parameter expressions are evaluated once when
  building the pipeline, including default values, but processing
  functions are not run. Calls and parameter expressions support
  tidy-evaluation injection with `!!` and `!!!`.

- exp:

  A supported glyclean experiment.

- pipeline:

  A `glyclean_pipeline` list.

## Value

`glyclean_pipeline()` and `get_pipeline()` return a `glyclean_pipeline`
list. `apply_pipeline()` returns a processed experiment.

## Details

A pipeline is a list of `glyclean_step` lists. Each step has exactly two
fields: `function` (the function object) and `parameters` (a named
list). Use `step[["function"]]` to inspect the function. Pipelines are
ordinary lists, not callable functions: use
`apply_pipeline(exp, pipeline)`.

Successful processing calls append to `metadata(exp)$pipeline` (or
`exp$meta_data$pipeline` for legacy experiments). Automatic operations
record the concrete operations they execute. QC summaries and plots do
not modify history. The experiment still contains only one abundance
assay; history contains parameters, not copies of intermediate assays.
Grouping values are frozen for replay. When `by` is supplied as a column
name, that original name is retained for printing the recorded step.

Replaying a history requires the same original input and compatible
package versions and RNG configuration. Each stochastic operation
records its explicit `seed` in `parameters`, including the default when
omitted. Replay passes these parameters directly to the operation. Steps
carry package-version information as an attribute; no random-generator
state is stored. Callback functions and parameter values are captured at
construction/recording time; external files, network side effects and
mutable references in arbitrary callbacks are not supported. Captured
values are checked recursively, including attributes and S4 slots;
environments, external pointers, and weak references are rejected.
Ordinary value objects and captured closures are copied for replay.
Recursive callback environments and excessively nested values are
rejected. Site-sequence annotation records the actual sequences.

## See also

[data_scales](https://glycoverse.github.io/glyclean/dev/reference/data_scales.md)
for working domains and supported input scales.

## Examples

``` r
pipeline <- glyclean_pipeline(normalize_total_area(), impute_min_prob(seed = 42))
cleaned <- apply_pipeline(glyexp::real_experiment, pipeline)
history <- get_pipeline(cleaned)
```
