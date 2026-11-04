# Transform Abundances to a Logarithmic Scale

Explicitly convert the abundance assay to natural, base-2, or base-10
logarithms and update its scale metadata.

## Usage

``` r
transform_log(x, base = 2)
```

## Arguments

- x:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object. Legacy `glyexp_experiment` objects are also supported.

- base:

  Logarithm base: `2` (default), `exp(1)` for natural logarithms, or
  `10`. These correspond to `"log2"`, `"log"`, and `"log10"` metadata.

## Value

A container of the same class as `x`, with its abundance assay and scale
metadata updated. Annotations and other metadata are preserved, and a
replayable step is appended to the preprocessing pipeline.

## Details

Generally, you do not need to call `transform_log()` in Glycoverse
workflows: packages including glystats and glyvis perform the log
transformations needed by their analyses or plots internally. Follow the
input contract of the downstream function. In particular, scale-aware
glystats functions use scale metadata, whereas glyvis plots that
internally compute `log2(x + 1)` expect raw abundances. This is not a
guarantee that every downstream function accepts pre-logged data. Use
this function when explicitly stored log abundances are required, for
example for export or an external analysis.

Input scale is read from metadata; absent metadata means `"raw"`.
Ordinary log inputs are converted directly between bases, without
exponentiation or taking a second logarithm. Matching input and output
scales leave the assay unchanged. `"logx"` is rejected because its
original abundances cannot be recovered from scale metadata alone.

Raw abundances must be non-negative. Zeros become `-Inf`, and `NA` is
preserved. Ordinary log inputs may contain finite negative or zero
coordinates and `-Inf` (zero abundance). `NaN` and positive infinity are
rejected on every input scale. No pseudocount or imputation is applied.
If zeros represent missing measurements, use
[`zero_to_na()`](https://glycoverse.github.io/glyclean/dev/reference/zero_to_na.md)
and an appropriate imputation method. Some downstream methods require
finite values. Logging a sparse raw assay may increase memory use
because implicit zeros become `-Inf`.

## See also

[data_scales](https://glycoverse.github.io/glyclean/dev/reference/data_scales.md),
[`glyclean_pipeline()`](https://glycoverse.github.io/glyclean/dev/reference/glyclean_pipeline.md)

## Examples

``` r
x <- SummarizedExperiment::SummarizedExperiment(
  assays = list(abundance = matrix(c(1, 2, 4, 8), nrow = 2))
)
logged <- transform_log(x)
SummarizedExperiment::assay(logged)
#>      [,1] [,2]
#> [1,]    0    2
#> [2,]    1    3
get_pipeline(logged)
#> 
#> ── <glyclean_pipeline> 1 step ──────────────────────────────────────────────────
#> 1. transform_log
#>    base = 2
```
