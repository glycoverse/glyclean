# Centered Log-Ratio Transformation

This function implements a glycoWork-compatible CLR preprocessing
strategy. Internally, the data are transformed on the `log2` scale
following `glycowork::clr_transformation()`, then back-transformed to
the original ratio space before returning the result.

## Usage

``` r
transform_clr(x, by = NULL, gamma = 0.1, group_scales = NULL, seed = 123)
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

  Integer seed for random number generation. Default is `123`.

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

## Glycoproteomics

For `GlycoproteomicSE` objects and legacy glycoproteomics experiments,
each unique combination of `protein` and `protein_site` is treated as a
separate composition. CLR centering, ALR reference selection, and
ALR-to-CLR fallback are performed independently within each site. Both
site identifiers must be present and non-missing. `by` continues to
specify sample groups.

The output preserves the original order of retained rows and their
annotations. Metadata field `coda_site_transforms` records `protein`,
`protein_site`, `transform`, and `reference` for each site; `reference`
is missing for CLR sites. `coda_transform` is `"clr"`, `"alr"`, or
`"mixed"` when sites use different methods, and `coda_reference`
contains all removed reference row names, or `NULL` when no references
are removed.

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

## Examples

``` r
x <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
x <- impute_zero(x[seq_len(6), ])
result <- transform_clr(x, gamma = 0)
S4Vectors::metadata(result)$coda_site_transforms
#> # A tibble: 3 × 4
#>   protein protein_site transform reference
#>   <chr>          <int> <chr>     <chr>    
#> 1 P08185           176 clr       NA       
#> 2 P04196           344 clr       NA       
#> 3 P10909           291 clr       NA       
```
