
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glyclean <a href="https://glycoverse.github.io/glyclean/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/glyclean)](https://CRAN.R-project.org/package=glyclean)
[![R-CMD-check](https://github.com/glycoverse/glyclean/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/glycoverse/glyclean/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/glycoverse/glyclean/graph/badge.svg)](https://app.codecov.io/gh/glycoverse/glyclean)
<!-- badges: end -->

Omics data cleaning and preprocessing is a critical yet cumbersome step.
**glyclean** helps you perform these tasks with ease, so you can focus
on the fun part: downstream analysis!

## Installation

You can install the latest release of glyclean from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("glycoverse/glyclean@*release")
```

Or install the development version:

``` r
remotes::install_github("glycoverse/glyclean")
```

## Documentation

-   🚀 Get started:
    [Here](https://glycoverse.github.io/glyclean/articles/glyclean.html)
-   📚 Reference:
    [Here](https://glycoverse.github.io/glyclean/reference/index.html)

## Role in `glycoverse`

As data preprocessing is an essential step in omics data analysis,
`glyclean` plays a central role in the `glycoverse` ecosystem. It serves
as the bridge between raw experimental data (imported via `glyread`) and
downstream analysis, enabling other packages like `glystats` to work
with clean, analysis-ready data.

## Example

``` r
library(glyexp)
library(glyclean)
#> 
#> Attaching package: 'glyclean'
#> The following object is masked from 'package:stats':
#> 
#>     aggregate

exp <- real_experiment
clean_exp <- auto_clean(exp)
#> 
#> ── Normalizing data ──
#> 
#> No QC samples found. Using default normalization method based on experiment
#> type.
#> Experiment type is "glycoproteomics". Using `normalize_median()`.
#> 
#> 
#> ── Removing variables with too many missing values ──
#> 
#> 
#> 
#> No QC samples found. Using all samples.
#> Applying preset "discovery"...
#> Total removed: 24 (0.56%) variables.
#> 
#> 
#> ── Imputing missing values ──
#> 
#> 
#> 
#> No QC samples found. Using default imputation method based on sample size.
#> Sample size <= 30, using `impute_sample_min()`.
#> 
#> 
#> ── Aggregating data ──
#> 
#> 
#> 
#> Aggregating to "gfs" level
#> 
#> 
#> ── Normalizing data again ──
#> 
#> 
#> 
#> No QC samples found. Using default normalization method based on experiment
#> type.
#> Experiment type is "glycoproteomics". Using `normalize_median()`.
#> 
#> 
#> ── Correcting batch effects ──
#> 
#> 
#> 
#> ℹ Batch column  not found in sample_info. Skipping batch correction.
```

Yes, that’s it! Calling the magical `auto_clean()` function will
automatically perform the following steps, in the most suitable way for
your data:

-   Normalization
-   Missing value filtering
-   Imputation
-   Batch effect correction

and other steps that are necessary for downstream analysis.
