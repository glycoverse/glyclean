# Centered Log-Ratio Normalization

This function implements a glycoWork-compatible CLR preprocessing
strategy. Internally, the data are transformed on the `log2` scale
following `glycowork::clr_transformation()`, then back-transformed to
the original ratio space before returning the result.

## Usage

``` r
normalize_clr(x, by = NULL, gamma = 0.1, group_scales = NULL)

# S3 method for class 'glyexp_experiment'
normalize_clr(x, by = NULL, gamma = 0.1, group_scales = NULL)

# S3 method for class 'matrix'
normalize_clr(x, by = NULL, gamma = 0.1, group_scales = NULL)

# Default S3 method
normalize_clr(x, by = NULL, gamma = 0.1, group_scales = NULL)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (for `glyexp_experiment` input)
  or a factor/vector with one value per sample.

- gamma:

  Standard deviation of the scale-uncertainty model on the `log2` scale.
  Default is `0.1`. Set to `0` for deterministic CLR.

- group_scales:

  Optional informed group scales. For binary comparisons, this can be a
  single positive ratio for the second group relative to the first, or
  two positive scales from which that ratio is derived. For multi-group
  data, provide a positive vector with one scale per group.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with CLR-transformed expression matrix. If
`x` is a matrix, returns a CLR-transformed matrix. The returned values
are back-transformed to the original ratio space. Zeros in the input
therefore remain zeros in the output.

## Details

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
