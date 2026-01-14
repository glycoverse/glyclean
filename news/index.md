# Changelog

## glyclean 0.11.0

### New features

- Added automatic UniProt sequence fetching when FASTA is NULL in
  [`add_site_seq()`](https://glycoverse.github.io/glyclean/reference/add_site_seq.md).
- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  now calls
  [`glyexp::standardize_variable()`](https://glycoverse.github.io/glyexp/reference/standardize_variable.html)
  after aggregation to ensure meaningful variable names.

## glyclean 0.10.1

### Minor improvements and fixes

- Fix a bug that the `qc_name` argument and `group_col` argument were
  ignored in
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md).
- The `qc_name` argument in `auto_xxx()` functions now can be NULL.

## glyclean 0.10.0

### New features

- Added a comprehensive suite of Quality Control (QC) plotting
  functions:
  - [`plot_missing_heatmap()`](https://glycoverse.github.io/glyclean/reference/plot_missing_heatmap.md):
    Binary heatmap of missing value patterns.
  - [`plot_missing_bar()`](https://glycoverse.github.io/glyclean/reference/plot_missing_bar.md):
    Bar plot of missing proportions by sample or variable.
  - [`plot_tic_bar()`](https://glycoverse.github.io/glyclean/reference/plot_tic_bar.md):
    Total intensity (TIC) bar plot by sample.
  - [`plot_rank_abundance()`](https://glycoverse.github.io/glyclean/reference/plot_rank_abundance.md):
    Protein rank abundance plot.
  - [`plot_int_boxplot()`](https://glycoverse.github.io/glyclean/reference/plot_int_boxplot.md):
    Log2-intensity boxplots by sample, with optional grouping.
  - [`plot_rle()`](https://glycoverse.github.io/glyclean/reference/plot_rle.md):
    Relative Log Expression (RLE) boxplots for detecting sample-wise
    bias.
  - [`plot_cv_dent()`](https://glycoverse.github.io/glyclean/reference/plot_cv_dent.md):
    CV density plot, with optional stratified groups.
  - [`plot_batch_pca()`](https://glycoverse.github.io/glyclean/reference/plot_batch_pca.md):
    PCA score plot colored by batch.
  - [`plot_rep_scatter()`](https://glycoverse.github.io/glyclean/reference/plot_rep_scatter.md):
    Scatter plots of replicate sample pairs with $R^{2}$ values.

### Minor improvements and fixes

- Add `pheatmap`, `ggplotify`, `factoextra`, and `patchwork` to
  `Suggests` to support enhanced QC visualizations.
- Document and provide examples for the new QC functions in the main
  vignette.

## glyclean 0.9.1

### Minor improvements and fixes

- Optimize the message printing of `auto_xxx()` functions.

## glyclean 0.9.0

### Breaking changes

- [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  has been redesigned to be more flexible and robust. It now calls
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md),
  [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md),
  [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md),
  [`auto_aggregate()`](https://glycoverse.github.io/glyclean/reference/auto_aggregate.md),
  and
  [`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/auto_correct_batch_effect.md)
  in sequence, depending on the experiment type.

### New features

- Add
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)
  to automatically normalize the data.
- Add
  [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md)
  to automatically remove variables with too many missing values.
- Add
  [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)
  to automatically impute the missing values.
- Add
  [`auto_aggregate()`](https://glycoverse.github.io/glyclean/reference/auto_aggregate.md)
  to automatically aggregate the data.
- Add
  [`auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/auto_correct_batch_effect.md)
  to automatically correct the batch effects.

### Minor improvements and fixes

- `remove_xxx()` functions now print a message about the number and
  proportion of variables removed.
- Add a `seed` argument to
  [`impute_miss_forest()`](https://glycoverse.github.io/glyclean/reference/impute_miss_forest.md)
  to make the imputation results reproducible.
- [`normalize_median()`](https://glycoverse.github.io/glyclean/reference/normalize_median.md)
  now issues a warning if any sample has a median value of 0, producing
  all NaNs in the result.
- [`correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/correct_batch_effect.md)
  now uses a new method to detect group-batch confounding. It uses
  Cramer’s V to measure the strength of the association between batch
  and group variables.
- Fix an error in
  [`correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/correct_batch_effect.md)
  examples.

## glyclean 0.8.1

### Minor improvements and fixes

- glyclean now depends on the CRAN version of glyrepr.

## glyclean 0.8.0

### Breaking changes

- Rename `remove_missing_variables()` to
  [`remove_rare()`](https://glycoverse.github.io/glyclean/reference/remove_rare.md).

### New features

- All functions in `glyclean` are generic now. This makes it easier to
  extend `glyclean` to other data types.
- Add
  [`remove_low_var()`](https://glycoverse.github.io/glyclean/reference/remove_low_var.md)
  for removing variables with low variance.
- Add
  [`remove_low_cv()`](https://glycoverse.github.io/glyclean/reference/remove_low_cv.md)
  for removing variables with low coefficient of variation.
- Add
  [`remove_constant()`](https://glycoverse.github.io/glyclean/reference/remove_constant.md)
  for removing constant variables.
- Add
  [`remove_low_expr()`](https://glycoverse.github.io/glyclean/reference/remove_low_expr.md)
  for removing variables with low expression or abundance.

### Minor improvements

- Update documentation to include the newly added functions.

## glyclean 0.7.1

### Minor improvements and bug fixes

- Fix bugs introduced by the breaking changes in `glyexp` 0.10.0.

## glyclean 0.7.0

### Breaking changes

- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  now has a new logic for aggregating glycoproteomics data. Instead of
  dropping all other columns, aggregate() now keeps columns
  intelligently. Common columns including “gene” will be kept in this
  way. This new logic has an important implication: columns added by
  functions like `glymotif::add_motif_lgl()` or
  `glydet::add_meta_properties()` will be kept.

### Minor improvements and bug fixes

- Better error message for
  [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  when the user tries to aggregate to a level demanding structure but
  the structure column is missing.
- Explicitly check if `sva` package is installed in
  [`correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/correct_batch_effect.md).
- Update dependencies to explicitly require `tibble` and `glyexp`.

## glyclean 0.6.4

### Minor improvements and bug fixes

- Update dependencies to depend on release versions of glycoverse
  packages.

## glyclean 0.6.3

### Bug fixes

- Fix bugs in documentation examples due to breaking changes introduced
  in `glyexp` 0.8.0.

## glyclean 0.6.2

### Bug fixes

- Add `scales` to `Imports`.

## glyclean 0.6.1

### Minor improvements

- Update the documentation of
  [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md).

### Bug fixes

- Fix a bug in
  [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  where it falled on experiments returned by most `glyread` functions.

## glyclean 0.6.0

### Major changes

- Add
  [`add_site_seq()`](https://glycoverse.github.io/glyclean/reference/add_site_seq.md)
  function to add site sequences to a glycopeptide experiment.

## glyclean 0.5.0

### Major changes

- Add `infer_protein()` function to resolve multiple protein assignments
  for glycopeptides.
- Add
  [`adjust_protein()`](https://glycoverse.github.io/glyclean/reference/adjust_protein.md)
  function to remove protein expression from glycopeptide expression.

### Minor improvements

- Rename the first argument of
  [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  from `x` to `exp` to be consistent with other functions.

## glyclean 0.4.0

### Major changes

- All data processing functions now accept matrices as input in addition
  to `glyexp_experiment` objects.
- The `by` parameter in data processing functions now accepts factors in
  addition to column names, enabling direct use with matrix inputs.
- API updated to support custom grouping factors for batch operations
  when working with matrices.

### Minor improvements

- Improved error messages for
  [`correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/correct_batch_effect.md)
  and
  [`detect_batch_effect()`](https://glycoverse.github.io/glyclean/reference/detect_batch_effect.md)
  when column names are not found in sample information.
- More meaningful error messages when providing column names as `batch`
  or `group` arguments with matrix inputs.
- Enhanced parameter validation and error handling across all modules.

### Bug fixes

- Fixed incorrect behavior of `by` argument in
  `filter_missing_variable()`.
- Resolved issues with parameter processing when using different input
  types.

## glyclean 0.3.0

- [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  now detects batch effects before batch correction.
- `to_level` argument is removed from
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md).
