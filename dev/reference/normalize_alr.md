# Additive Log-Ratio Normalization

This function implements the ALR preprocessing strategy described in
DOI: 10.1038/s41467-025-56249-3. The data are transformed on the `log2`
scale relative to an automatically selected reference glycan, which is
removed from the output.

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
  ranking and for CLR fallback.

- gamma:

  Standard deviation of the CLR scale-uncertainty model used when ALR
  falls back to CLR. Default is `0.1`.

- group_scales:

  Optional known group-level scale differences passed through to CLR
  fallback. See
  [`normalize_clr()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_clr.md).

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with an ALR-transformed expression matrix.
If `x` is a matrix, returns an ALR-transformed matrix. When ALR
succeeds, the reference glycan is excluded from the result and the
output therefore has one fewer row than the input. When ALR falls back
to CLR, the returned object keeps the original dimensions. The returned
values are back-transformed to the original ratio space, corresponding
to `x / x_ref`. Zeros in non-reference glycans therefore remain zeros in
the output.

## Details

Candidate reference glycans are ranked using the product of their
Procrustes correlation to the corresponding CLR geometry and the inverse
of their between-group variance. If the best candidate has Procrustes
correlation below `0.9` or between-group variance above `0.1`, ALR is
abandoned and CLR is returned instead, matching the fallback rule
described in DOI: 10.1038/s41467-025-56249-3.

The reference search only considers glycans that are strictly positive
in all samples, because the reference must remain finite on the `log2`
scale. Non-reference glycans may still contain zeros; those entries
become infinite after transformation.
