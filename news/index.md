# Changelog

## glyclean 0.17.1

### Minor improvements and bug fixes

- [`impute_svd()`](https://glycoverse.github.io/glyclean/reference/impute_svd.md)
  documentation uses LaTeX-compatible mathematical notation so the PDF
  reference manual builds with pdflatex.
- SE preprocessing preserves ordinary `variable`, `sample`, `.variable`,
  and `.sample` annotations instead of treating them as axis
  identifiers. Aggregation and rank-abundance QC use the assay axes, and
  `by = "sample"` reads ordinary SE annotations. (#54)
- Subsetting older glyco SE objects fills in the historical raw scale
  before current glyexp validation. (#54)

## glyclean 0.17.0

### Breaking changes

- Preprocessing and QC now honor `raw`, `log`, `log2`, and `log10`
  scale, restoring the input scale after processing and preserving
  original values when filtering. (#41)
- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md),
  [`impute_bpca()`](https://glycoverse.github.io/glyclean/reference/impute_bpca.md),
  [`impute_fw_knn()`](https://glycoverse.github.io/glyclean/reference/impute_fw_knn.md),
  [`impute_svd()`](https://glycoverse.github.io/glyclean/reference/impute_svd.md),
  [`impute_sw_knn()`](https://glycoverse.github.io/glyclean/reference/impute_sw_knn.md),
  and
  [`normalize_vsn()`](https://glycoverse.github.io/glyclean/reference/normalize_vsn.md)
  now use `seed = 123` for all random operations, replacing the backend
  default of `362436069`. Pass `seed` directly to reproduce a chosen
  seed. (#35, \#36)
- [`auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.md)
  now always uses site-wise CLR for glycoproteomics, and
  [`transform_alr()`](https://glycoverse.github.io/glyclean/reference/transform_alr.md)
  rejects glycoproteomics input. (#34, \#37)
- [`impute_zero()`](https://glycoverse.github.io/glyclean/reference/impute_zero.md)
  now requires raw abundances. Supply an experiment on the `raw` scale
  for zero imputation. (#41)
- [`normalize_vsn()`](https://glycoverse.github.io/glyclean/reference/normalize_vsn.md)
  now marks its output with `scale = "logx"`. Use the returned scale
  metadata for downstream processing. (#41)
- [`transform_alr()`](https://glycoverse.github.io/glyclean/reference/transform_alr.md),
  [`transform_clr()`](https://glycoverse.github.io/glyclean/reference/transform_clr.md),
  and
  [`auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.md)
  now return log2-ratio coordinates instead of exponentiated ratios and
  set `scale = "logx"` in experiment metadata. (#39, \#41)
- [`transform_alr()`](https://glycoverse.github.io/glyclean/reference/transform_alr.md),
  [`transform_clr()`](https://glycoverse.github.io/glyclean/reference/transform_clr.md),
  and
  [`auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.md)
  now reject zero abundances and require finite, strictly positive
  input. (#39)
- [`zero_to_na()`](https://glycoverse.github.io/glyclean/reference/zero_to_na.md)
  converts `-Inf` to `NA` on ordinary log scales while preserving finite
  log-scale zeros. (#41)

### New features

- [`adjust_protein()`](https://glycoverse.github.io/glyclean/reference/adjust_protein.md)
  gains `pro_scale` to describe protein matrix values independently of
  the experiment scale, supporting `raw`, `log`, `log2`, and `log10`
  inputs while preserving the experiment representation and recording
  replayable parameters. Both inputs reject `logx` scale. (#40)
- New
  [`glyclean_pipeline()`](https://glycoverse.github.io/glyclean/reference/glyclean_pipeline.md),
  [`apply_pipeline()`](https://glycoverse.github.io/glyclean/reference/glyclean_pipeline.md),
  and
  [`get_pipeline()`](https://glycoverse.github.io/glyclean/reference/glyclean_pipeline.md)
  create, replay, and retrieve preprocessing pipelines stored in
  experiment metadata while retaining a single assay. Automatic
  preprocessing records the selected operations, seeds are stored as
  step parameters, and sequence annotation inputs are frozen for replay.
  Pipelines print numbered steps with compact parameter summaries. (#36)
- New
  [`transform_log()`](https://glycoverse.github.io/glyclean/reference/transform_log.md)
  explicitly converts abundances to natural, base-2, or base-10
  logarithms, updates scale metadata, and records a replayable pipeline
  step. (#43)
- New
  [`zero_to_na()`](https://glycoverse.github.io/glyclean/reference/zero_to_na.md)
  replaces exact zeros in the abundance assay on the raw scale and
  negative infinity on ordinary log scales with missing values, and
  records a replayable pipeline step. (#38)

### Minor improvements and bug fixes

- Preprocessing accepts older glyco SE objects without `scale` metadata
  and records `"raw"` in returned objects, including filtering, skipped
  operations, and empty pipelines, without changing the input.
  Processing rejects explicitly invalid scales. (#53)
- Grouped preprocessing now tracks samples by column position,
  preserving values and order with missing or duplicated sample names;
  unused group levels are ignored and missing grouping values are
  rejected.
- Filtering functions preserve the correct rows and annotations when a
  plain `SummarizedExperiment` has absent or duplicate row names,
  retaining the original abundance representation. (#44)
- Protein adjustment and internal SE subsetting keep feature annotations
  aligned by position when feature names are missing or duplicated.
- [`add_site_seq()`](https://glycoverse.github.io/glyclean/reference/add_site_seq.md)
  records replayable empty sequence snapshots and rejects missing or
  whitespace-only sequences after normalization, including fetched
  sequences. (#45)
- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  sums known-log input without exponentiating extreme abundances, while
  custom aggregation functions receive raw values. All-missing sums
  remain zero (`-Inf` on ordinary log scales). (#41)
- [`auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.md)
  and
  [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)
  now select preprocessing strategies for legacy experiment objects
  without triggering deprecated dimension-accessor warnings.
- [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md)
  preserves the actual filtered rows when feature names are absent or
  duplicated instead of selecting the original object again by name.
  Rare-variable filtering ignores unused factor levels so recorded
  preset steps can be replayed with the original group annotations.
- [`detect_batch_effect()`](https://glycoverse.github.io/glyclean/reference/detect_batch_effect.md)
  now uses the same `log2(raw + 1e-6)` working domain as batch
  correction, correcting raw-domain ANOVA results; ordinary-scale
  correction restores the input scale and `logx` correction retains
  negative coordinates. (#41)
- [`impute_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_sample_min.md)
  and
  [`impute_half_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_half_sample_min.md)
  now preserve all-missing columns as missing and retain matrix
  dimensions for singleton inputs. (#41)
- [`normalize_median()`](https://glycoverse.github.io/glyclean/reference/normalize_median.md)
  and
  [`normalize_median_abs()`](https://glycoverse.github.io/glyclean/reference/normalize_median_abs.md)
  documentation now describes rescaling to the geometric mean of sample
  medians, matching the existing limma algorithms. (#41)
- [`transform_alr()`](https://glycoverse.github.io/glyclean/reference/transform_alr.md),
  [`transform_clr()`](https://glycoverse.github.io/glyclean/reference/transform_clr.md),
  and
  [`auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.md)
  now return log2-ratios and validate that abundances, `group_scales`,
  and derived two-group ratios are finite and strictly positive. (#39)

## glyclean 0.16.0

### Breaking changes

- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  now defaults to `"gfs"` for glycoproteomics data with
  `glycan_structure`, preserving distinct structures instead of
  combining them by composition. Set `to_level = "gf"` to retain
  composition-level aggregation. (#28)
- [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  now aggregates glycomics data automatically. Use the individual
  preprocessing functions to retain unaggregated features. (#28)

### New features

- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  gains an `f` argument for custom aggregation functions such as
  [`mean()`](https://rdrr.io/r/base/mean.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html), with
  [`sum()`](https://rdrr.io/r/base/sum.html) remaining the default.
  (#27, \#30)
- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  now supports glycomics levels `"g"` and `"gs"`, defaulting to `"gs"`
  when `glycan_structure` is present and `"g"` otherwise.
  Structure-aware defaults also apply to glycoproteomics data. (#28)
- Stochastic preprocessing functions now accept a reproducibility
  `seed`, with
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  forwarding it through automatic imputation. (#29)

### Minor improvements and bug fixes

- [`auto_aggregate()`](https://glycoverse.github.io/glyclean/reference/auto_aggregate.md)
  is deprecated in favor of
  [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md).
  (#28)
- [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  now includes aggregation in its glycomics preprocessing pipeline.
  (#28)
- `correct_batch_effect(method = "limma")` now preserves the specified
  biological group effects. (#31)
- [`detect_batch_effect()`](https://glycoverse.github.io/glyclean/reference/detect_batch_effect.md)
  now tests batch effects after accounting for group in unbalanced
  designs, using the same complete samples for both models. (#31)

## glyclean 0.15.2

- Documentation and vignettes now recommend `GlycomicSE` and
  `GlycoproteomicSE` containers with `SummarizedExperiment` accessors
  for Stage II of glycoverse/glyexp#15. (#26)

## glyclean 0.15.1

- Preprocessing and QC functions now work consistently across legacy
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  and current `SummarizedExperiment` containers.

## glyclean 0.15.0

### Breaking changes

- Deprecated automatic-preprocessing arguments have been removed:
  `qc_name` from
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md),
  [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md),
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md),
  and
  [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md);
  `to_try` from
  [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)
  and
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md);
  and `impute_to_try` and `normalize_to_try` from
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md).
  (#20)
- Normalization, imputation, filtering, CoDA, and batch-correction
  functions now require a
  [`glyexp::GlycomicSE`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or legacy
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object; matrix inputs error and should first be wrapped in a supported
  container. (#18, \#19)

### New features

- All preprocessing and QC functions now accept
  [`glyexp::GlycomicSE`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  legacy
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html),
  and plain `SummarizedExperiment` objects (for experiment type
  `"others"`), while preserving their class and metadata. (#19, \#24)

### Minor improvements and bug fixes

- [`aggregate()`](https://glycoverse.github.io/glyclean/reference/aggregate.md)
  now processes large glycoproteomics experiments substantially faster
  while preserving group order and metadata. (#21)
- [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  now removes glycoproteomics variables with excessive missingness
  before the first normalization step. (#23)

## glyclean 0.14.1

### Minor improvements and bug fixes

- [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  is now out-of-box without needing to install other packages (#15).
- Reimplement
  [`impute_min_prob()`](https://glycoverse.github.io/glyclean/reference/impute_min_prob.md)
  to avoid the dependency on `imputeLCMD` package (#16).

## glyclean 0.14.0

We have made significant updates to
[`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md).
QC-related behaviors are now removed from
[`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md),
[`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md),
and
[`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md),
because we realized that depending on CVs in QC samples to determine the
imputation or normalization strategy is not robust.

### Breaking changes

- The `info` parameter in `auto_xxx()` functions is removed. This should
  not be a problem because this parameter was only used internally
  (#13).

### New features

- [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)
  now uses a different strategy. When sample size \< 30, use
  [`impute_min_prob()`](https://glycoverse.github.io/glyclean/reference/impute_min_prob.md).
  When 30 \<= sample size \< 100, use
  [`impute_bpca()`](https://glycoverse.github.io/glyclean/reference/impute_bpca.md)
  for glycomics data and
  [`impute_min_prob()`](https://glycoverse.github.io/glyclean/reference/impute_min_prob.md)
  for glycoproteomics data. When sample size \>= 100, use
  [`impute_miss_forest()`](https://glycoverse.github.io/glyclean/reference/impute_miss_forest.md)
  for glycomics data and
  [`impute_bpca()`](https://glycoverse.github.io/glyclean/reference/impute_bpca.md)
  for glycoproteomics data (#8).
- [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)
  and
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)
  do not rely on QC samples to determine the strategy (#8, \#9).
- [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md)
  does not take into account the QC samples anymore (a1cb616).

### Minor improvements and bug fixes

- `qc_name` argument in
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md),
  [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md),
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md),
  and
  [`auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.md)
  is deprecated (#8, \#9, \#10).
- `to_try` argument in
  [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)
  and
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)
  is deprecated. `impute_to_try` and `normalize_to_try` arguments in
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  are also deprecated (#8, \#9).
- [`auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.md)
  and
  [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)
  now support fallbacks for experiments with “others” type (#8, \#9).
- Fix the bug that `batch_col` argument in
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  is ignored (#12).
- Optimize message printing in `auto_xxx()` functions (#14).

## glyclean 0.13.0

### New features

- [`transform_clr()`](https://glycoverse.github.io/glyclean/reference/transform_clr.md)
  and
  [`transform_alr()`](https://glycoverse.github.io/glyclean/reference/transform_alr.md)
  now align with the methods described in DOI:
  10.1038/s41467-025-56249-3.
- [`auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.md)
  now only performs total abundance normalization for glycomics data.
- Add
  [`auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.md)
  to use the CoDA strategy in glycowork for glycomics data.

## glyclean 0.12.2

### New features

- Add
  [`transform_clr()`](https://glycoverse.github.io/glyclean/reference/transform_clr.md)
  for Centered Log-Ratio (CLR) transformation.
- Add
  [`transform_alr()`](https://glycoverse.github.io/glyclean/reference/transform_alr.md)
  for Additive Log-Ratio (ALR) transformation.

### Minor improvements and bug fixes

- Update documentation of
  [`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
  for QC-related behaviors.

## glyclean 0.12.1

### Minor improvements and bug fixes

- Update dependency strategy to use the r-universe repository.

## glyclean 0.12.0

### New features

- Add `method` parameter to
  [`correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/correct_batch_effect.md)
  supporting limma method for batch correction.

### Minor improvements and bug fixes

- Fix method handling in
  [`correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/correct_batch_effect.md)
  for glyexp_experiment method.
- Fix long example line to avoid Rd line width note.

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
    Scatter plots of replicate sample pairs with $`R^2`$ values.

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
