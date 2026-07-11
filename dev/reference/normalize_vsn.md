# Variance Stabilizing Normalization

This function is a wrapper around
[`limma::normalizeVSN()`](https://rdrr.io/pkg/limma/man/normalizeVSN.html).
Evidence proved that this method performs well in reducing noise and
boosting differential expression detection. Log-transformation is not
needed for downstream statistical analysis, for this normalization
method performs a log-like transformation internally. Due to the same
reason, fold change estimates will be severely distorted. Please use
this method with caution. See [this
paper](https://doi.org/10.1093/bib/bbw095) for more information.

## Usage

``` r
normalize_vsn(x, by = NULL, ...)
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
  [`limma::normalizeVSN()`](https://rdrr.io/pkg/limma/man/normalizeVSN.html).

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with a normalized expression matrix. Glyco SE inputs return the
same subclass.

## Details

At least 42 variables should be provided for this method. This follows
the convention of the `vsn` package.
