# Automatical CoDA transformation

For glycoproteomics, always call
[`transform_clr()`](https://glycoverse.github.io/glyclean/dev/reference/transform_clr.md)
to transform each protein site independently, regardless of the number
of features at the site. For other supported containers, call
[`transform_alr()`](https://glycoverse.github.io/glyclean/dev/reference/transform_alr.md)
when the data has more than 50 variables and
[`transform_clr()`](https://glycoverse.github.io/glyclean/dev/reference/transform_clr.md)
otherwise, following glycowork.

## Usage

``` r
auto_coda(x, by = NULL, gamma = 0.1, group_scales = NULL, seed = 123)
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

- seed:

  Non-negative integer seed for random number generation. Default is
  `123`. The caller's random state is restored, including on errors.

## Value

The input container with a CoDA-transformed expression matrix and its
class preserved. Glycoproteomics always uses site-wise CLR and retains
all rows. Other supported containers use ALR if there are more than 50
variables, or CLR otherwise.

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

## Glycoproteomics

For `GlycoproteomicSE` objects and legacy glycoproteomics experiments,
each unique combination of `protein` and `protein_site` is treated as a
separate composition. CLR centering is performed independently within
each site. ALR is not supported for glycoproteomics; use
[`transform_clr()`](https://glycoverse.github.io/glyclean/dev/reference/transform_clr.md)
or `auto_coda()`, which always uses CLR. Both site identifiers must be
present and non-missing. `by` continues to specify sample groups.

The output preserves all rows, their original order, and their
annotations. Metadata field `coda_site_transforms` records `protein`,
`protein_site`, `transform`, and `reference` for each site; `reference`
is always missing. `coda_transform` is `"clr"`, and `coda_reference` is
`NULL`.

With `gamma = 0`, a site containing a single feature returns one for
positive abundances and zero for zeros. Samples with no positive
abundance at a site remain zero for that site.

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
