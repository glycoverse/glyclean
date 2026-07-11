# Half Sample Minimum Imputation

Impute missing values with half of the minimum value of the
corresponding sample. This method assumes that missing values are MCAR,
i.e. missing values are induced by an ion below the detection limit.
Compared to
[`impute_sample_min()`](https://glycoverse.github.io/glyclean/dev/reference/impute_sample_min.md),
this method is more conservative.

## Usage

``` r
impute_half_sample_min(x)
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

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with missing values imputed. Glyco SE inputs return the same
subclass.
