# Additive Log-Ratio Transformation

This function implements a glycoWork-compatible ALR preprocessing
strategy. Internally, the data are transformed relative to an
automatically selected reference glycan using glycoWork-style ALR logic.
The `log2`-ratio coordinates are returned directly, with
`scale = "logx"` in metadata.

## Usage

``` r
transform_alr(x, by = NULL, gamma = 0.1, group_scales = NULL, seed = 123)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- by:

  Either a column name in `sample_info` or a factor/vector with one
  value per sample.

- gamma:

  Standard deviation of the scale-uncertainty model on the `log2` scale.
  Default is `0.1`. Set to `0` for deterministic transformation.

- group_scales:

  Optional informed group scales. For binary comparisons, this can be a
  single positive ratio for the second group relative to the first, or
  two positive scales from which that ratio is derived. For multi-group
  data, provide a positive vector with one scale per group. All scales
  and any derived two-group ratio must be finite and strictly positive.

- seed:

  Non-negative integer seed for random number generation. Default is
  `123`. The caller's random state is restored, including on errors.

## Value

A container of the same class as `x`, with an ALR-transformed expression
matrix. When ALR succeeds, the reference glycan is excluded from the
result and the output therefore has one fewer row. CLR fallback keeps
the original dimensions. Fallback records CLR and starts from the
supplied seed, so its random stream is independent of reference
assessment. Glycoproteomics input is not supported. The returned values
are `log2`-ratio coordinates with `scale = "logx"`, corresponding to
`log2(x / x_ref)` when `gamma = 0` and no informed scales are supplied.

## Algorithmic details

The stochastic branch follows `glycowork`: when `gamma > 0`, CLR noise
is sampled per feature-sample entry rather than once per sample. Without
informed scales, this corresponds to jittering around the
sample-specific `log2` geometric mean of the components. The result
remains on the log-ratio scale, without exponentiation.

The `group_scales` argument is interpreted in the same way as
`glycowork`'s `custom_scale`. For exactly two groups, provide the
total-signal ratio of the second group relative to the first, either
directly as a scalar or implicitly through two per-group scales. For
multi-group data, provide one positive scale per group; these scales are
used only in the stochastic branch.

## Input requirements

CoDA accepts `raw`, `log`, `log2`, and `log10` inputs, and rejects
`logx`. On raw input, abundances must be finite and strictly positive.
On ordinary log input, finite zero and negative coordinates are valid;
`-Inf` represents zero abundance and is rejected. Missing values and
other non-finite values are rejected on all input scales; no
pseudo-count or automatic imputation is applied. If zero abundances
represent missing observations, first call
[`zero_to_na()`](https://glycoverse.github.io/glyclean/reference/zero_to_na.md),
then use an appropriate imputation function to obtain positive values.
Choose the imputation method for your data;
[`impute_zero()`](https://glycoverse.github.io/glyclean/reference/impute_zero.md)
is not suitable before CoDA. Entirely missing features or samples may
need filtering rather than imputation.

## Glycoproteomics

For `GlycoproteomicSE` objects and legacy glycoproteomics experiments,
each unique combination of `protein` and `protein_site` is treated as a
separate composition. CLR centering is performed independently within
each site. ALR is not supported for glycoproteomics; use
[`transform_clr()`](https://glycoverse.github.io/glyclean/reference/transform_clr.md)
or
[`auto_coda()`](https://glycoverse.github.io/glyclean/reference/auto_coda.md),
which always uses CLR. Both site identifiers must be present and
non-missing. `by` continues to specify sample groups.

The output preserves all rows, their original order, and their
annotations.

With `gamma = 0` and no informed scales, a site containing a single
positive feature returns zero on the log-ratio scale.

## Motif quantification

If you intend to use ALR/CLR transformation with motif quantification,
you should apply the transformation after motif quantification rather
than before. In another words, the recommended workflow is:

1.  Perform data preprocessing.

    clean_exp <- auto_clean(exp)

1.  Perform motif quantification on the cleaned experiment.

    motif_exp <- glydet::quantify_motifs(clean_exp, motifs)

1.  Perform ALR/CLR transformation on the motif-quantified experiment.

    coda_motif_exp <- auto_coda(motif_exp, by = "group", gamma = 0.1)

1.  Proceed with downstream analyses on the transformed motif
    experiment.

    dea_res <- glystats::gly_ttest(coda_motif_exp)

## See also

[data_scales](https://glycoverse.github.io/glyclean/reference/data_scales.md)
for working domains and supported input scales.
