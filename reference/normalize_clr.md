# Centered Log-Ratio Normalization

This function implements the CLR preprocessing strategy described in
DOI: 10.1038/s41467-025-56249-3. Each sample is transformed on the
`log2` scale and centered by the geometric mean of its non-zero
components. Zeros are therefore allowed in the input and remain `-Inf`
after transformation, while negative values are rejected.

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
  or a factor/vector with one value per sample. Used only when
  `group_scales` is supplied.

- gamma:

  Standard deviation of the scale-uncertainty model on the `log2` scale.
  Default is `0.1`. Set to `0` for deterministic CLR.

- group_scales:

  Optional known group-level scale differences. For binary comparisons,
  this can be a single positive ratio for the second group relative to
  the first. For multi-group data, provide a named positive vector with
  one scale per group.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with CLR-transformed expression matrix. If
`x` is a matrix, returns a CLR-transformed matrix. The returned values
are back-transformed to the original ratio space, corresponding to
`x / g(x)`. Zeros in the input therefore remain zeros in the output.

## Details

The default implementation includes a scale-uncertainty model with
`gamma = 0.1`, matching the paper. For each sample, the `log2`
geometric-mean center is drawn from `N(mu, gamma^2)`, where `mu` is the
`log2` geometric mean of the non-zero components. Setting `gamma = 0`
disables the stochastic part and yields a deterministic transform.

If known group-level scale differences are available, they can be
supplied via `group_scales` together with `by`. In that case, the
group-specific scale shift is added back on the `log2` scale, following
the informed-scale model described in DOI: 10.1038/s41467-025-56249-3.
