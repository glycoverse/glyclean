# LoessCyc Normalization

This function is a wrapper around
[`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html)
with `method = "pairs"`. Each pair of columns is normalized mutually to
each other. See [this paper](https://doi.org/10.1093/bib/bbw095) for
more information. Also see
[`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html).

## Usage

``` r
normalize_loesscyc(x, by = NULL, ...)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  and
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  objects are also supported.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Optional. If provided,
  the normalization will be performed within each group.

- ...:

  Additional arguments to pass to
  [`limma::normalizeCyclicLoess()`](https://rdrr.io/pkg/limma/man/normalizeCyclicLoess.html).

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix. Glyco SE inputs return the
same subclass.
