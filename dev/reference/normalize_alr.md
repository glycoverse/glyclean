# Additive Log-Ratio Normalization

This function implements a glycoWork-compatible ALR preprocessing
strategy. Internally, the data are transformed relative to an
automatically selected reference glycan using glycoWork-style ALR logic,
then back-transformed to the original ratio space before returning the
result.

## Usage

``` r
normalize_alr(x, by = NULL, gamma = 0.1, group_scales = NULL)

# S3 method for class 'glyexp_experiment'
normalize_alr(x, by = NULL, gamma = 0.1, group_scales = NULL)

# S3 method for class 'matrix'
normalize_alr(x, by = NULL, gamma = 0.1, group_scales = NULL)

# Default S3 method
normalize_alr(x, by = NULL, gamma = 0.1, group_scales = NULL)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- by:

  Either a column name in `sample_info` (for `glyexp_experiment` input)
  or a factor/vector with one value per sample. Used for reference
  ranking and group-aware ALR/CLR behavior.

- gamma:

  Standard deviation of the glycoWork-style uncertainty model. Default
  is `0.1`.

- group_scales:

  Optional informed group scales passed through to the glycoWork-style
  ALR/CLR logic. See
  [`normalize_clr()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_clr.md).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with an ALR-transformed expression matrix.
If `x` is a matrix, returns an ALR-transformed matrix. When ALR
succeeds, the reference glycan is excluded from the result and the
output therefore has one fewer row than the input. When ALR falls back
to CLR, the returned object keeps the original dimensions. The returned
values are back-transformed to the original ratio space, corresponding
to `x / x_ref`.

## Details

Candidate reference glycans are ranked using the product of their
Procrustes correlation to the corresponding CLR geometry and the inverse
of their between-group variance, using the same fallback thresholds as
`glycowork`. If the best candidate has Procrustes correlation below
`0.9` or between-group variance above `0.1`, ALR is abandoned and CLR is
returned.

The reference search only considers glycans that are strictly positive
in all samples, because the reference must remain finite on the `log2`
scale. Non-reference glycans may still contain zeros; those entries
remain zero in the returned ratio-space output.

When `gamma > 0`, successful ALR transformations also include
glycoWork-style uncertainty on the reference scale rather than reserving
`gamma` only for CLR fallback.
