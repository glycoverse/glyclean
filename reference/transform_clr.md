# Centered Log-Ratio Transformation

This function implements a glycoWork-compatible CLR preprocessing
strategy. Internally, the data are transformed on the `log2` scale
following `glycowork::clr_transformation()`, then back-transformed to
the original ratio space before returning the result.

## Usage

``` r
transform_clr(x, by = NULL, gamma = 0.1, group_scales = NULL)
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
  data, provide a positive vector with one scale per group.

## Value

A container of the same class as `x`, with a transformed expression
matrix. The returned values are back-transformed to the original ratio
space. Zeros in the input therefore remain zeros in the output.

## Algorithmic details

The stochastic branch follows `glycowork`: when `gamma > 0`, CLR noise
is sampled per feature-sample entry rather than once per sample. Without
informed scales, this corresponds to jittering around the
sample-specific `log2` geometric mean of the non-zero components and
then returning to ratio space.

The `group_scales` argument is interpreted in the same way as
`glycowork`'s `custom_scale`. For exactly two groups, provide the
total-signal ratio of the second group relative to the first, either
directly as a scalar or implicitly through two per-group scales. For
multi-group data, provide one positive scale per group; these scales are
used only in the stochastic branch.

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
