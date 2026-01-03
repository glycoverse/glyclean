# glyclean 0.10.0

## New features

- Added a comprehensive suite of Quality Control (QC) plotting functions:
    - `plot_missing_heatmap()`: Binary heatmap of missing value patterns.
    - `plot_missing_bar()`: Bar plot of missing proportions by sample or variable.
    - `plot_tic_bar()`: Total intensity (TIC) bar plot by sample.
    - `plot_rank_abundance()`: Protein rank abundance plot.
    - `plot_int_boxplot()`: Log2-intensity boxplots by sample, with optional grouping.
    - `plot_rle()`: Relative Log Expression (RLE) boxplots for detecting sample-wise bias.
    - `plot_cv_dent()`: CV density plot, with optional stratified groups.
    - `plot_batch_pca()`: PCA score plot colored by batch.
    - `plot_rep_scatter()`: Scatter plots of replicate sample pairs with $R^2$ values.

## Minor improvements and fixes

- Add `pheatmap`, `ggplotify`, `factoextra`, and `patchwork` to `Suggests` to support enhanced QC visualizations.
- Document and provide examples for the new QC functions in the main vignette.

# glyclean 0.9.1

## Minor improvements and fixes

- Optimize the message printing of `auto_xxx()` functions.

# glyclean 0.9.0

## Breaking changes

- `auto_clean()` has been redesigned to be more flexible and robust. It now calls `auto_normalize()`, `auto_remove()`, `auto_impute()`, `auto_aggregate()`, and `auto_correct_batch_effect()` in sequence, depending on the experiment type.

## New features

- Add `auto_normalize()` to automatically normalize the data.
- Add `auto_remove()` to automatically remove variables with too many missing values.
- Add `auto_impute()` to automatically impute the missing values.
- Add `auto_aggregate()` to automatically aggregate the data.
- Add `auto_correct_batch_effect()` to automatically correct the batch effects.

## Minor improvements and fixes

- `remove_xxx()` functions now print a message about the number and proportion of variables removed.
- Add a `seed` argument to `impute_miss_forest()` to make the imputation results reproducible.
- `normalize_median()` now issues a warning if any sample has a median value of 0, producing all NaNs in the result.
- `correct_batch_effect()` now uses a new method to detect group-batch confounding. It uses Cramer's V to measure the strength of the association between batch and group variables.
- Fix an error in `correct_batch_effect()` examples.

# glyclean 0.8.1

## Minor improvements and fixes

- glyclean now depends on the CRAN version of glyrepr.

# glyclean 0.8.0

## Breaking changes

- Rename `remove_missing_variables()` to `remove_rare()`.

## New features

- All functions in `glyclean` are generic now. This makes it easier to extend `glyclean` to other data types.
- Add `remove_low_var()` for removing variables with low variance.
- Add `remove_low_cv()` for removing variables with low coefficient of variation.
- Add `remove_constant()` for removing constant variables.
- Add `remove_low_expr()` for removing variables with low expression or abundance.

## Minor improvements

- Update documentation to include the newly added functions.

# glyclean 0.7.1

## Minor improvements and bug fixes

- Fix bugs introduced by the breaking changes in `glyexp` 0.10.0.

# glyclean 0.7.0

## Breaking changes

- `aggregate()` now has a new logic for aggregating glycoproteomics data. Instead of dropping all other columns, aggregate() now keeps columns intelligently. Common columns including "gene" will be kept in this way. This new logic has an important implication: columns added by functions like `glymotif::add_motif_lgl()` or `glydet::add_meta_properties()` will be kept.

## Minor improvements and bug fixes

- Better error message for `aggregate()` when the user tries to aggregate to a level demanding structure but the structure column is missing.
- Explicitly check if `sva` package is installed in `correct_batch_effect()`.
- Update dependencies to explicitly require `tibble` and `glyexp`.

# glyclean 0.6.4

## Minor improvements and bug fixes

-  Update dependencies to depend on release versions of glycoverse packages.

# glyclean 0.6.3

## Bug fixes

- Fix bugs in documentation examples due to breaking changes introduced in `glyexp` 0.8.0.

# glyclean 0.6.2

## Bug fixes

- Add `scales` to `Imports`.

# glyclean 0.6.1

## Minor improvements

- Update the documentation of `aggregate()`.

## Bug fixes

- Fix a bug in `aggregate()` where it falled on experiments returned by most `glyread` functions.

# glyclean 0.6.0

## Major changes

- Add `add_site_seq()` function to add site sequences to a glycopeptide experiment.

# glyclean 0.5.0

## Major changes

- Add `infer_protein()` function to resolve multiple protein assignments for glycopeptides.
- Add `adjust_protein()` function to remove protein expression from glycopeptide expression.

## Minor improvements

- Rename the first argument of `aggregate()` from `x` to `exp` to be consistent with other functions.

# glyclean 0.4.0

## Major changes

- All data processing functions now accept matrices as input in addition to `glyexp_experiment` objects.
- The `by` parameter in data processing functions now accepts factors in addition to column names, 
  enabling direct use with matrix inputs.
- API updated to support custom grouping factors for batch operations when working with matrices.

## Minor improvements

- Improved error messages for `correct_batch_effect()` and `detect_batch_effect()` when column 
  names are not found in sample information.
- More meaningful error messages when providing column names as `batch` or `group` arguments 
  with matrix inputs.
- Enhanced parameter validation and error handling across all modules.

## Bug fixes

- Fixed incorrect behavior of `by` argument in `filter_missing_variable()`.
- Resolved issues with parameter processing when using different input types.

# glyclean 0.3.0

- `auto_clean()` now detects batch effects before batch correction.
- `to_level` argument is removed from `auto_clean()`.
