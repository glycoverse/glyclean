# LoessF Normalization

This function is a wrapper around
[`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html)
with `method = "fast"`. Each column is simply normalized to a reference
array, the reference array being the average of all the arrays. See
[this paper](https://doi.org/10.1093/bib/bbw095) for more information.
Also see
[`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html).

## Usage

``` r
normalize_loessf(x, by = NULL, ...)
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

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

- ...:

  Additional arguments to pass to
  [`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html).

## Value

A container of the same class as `x`, with a normalized expression
matrix.
