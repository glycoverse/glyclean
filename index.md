# glyclean

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

- 🚀 Get started:
  [Here](https://glycoverse.github.io/glyclean/articles/glyclean.html)
- 📚 Reference:
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
#> ℹ Normalizing data (Median)
#> ✔ Normalizing data (Median) [70ms]
#> 
#> ℹ Removing variables with >50% missing values
#> ✔ Removing variables with >50% missing values [38ms]
#> 
#> ℹ Imputing missing values
#> ℹ Sample size <= 30, using sample minimum imputation
#> ℹ Imputing missing values✔ Imputing missing values [12ms]
#> 
#> ℹ Aggregating data
#> ✔ Aggregating data [422ms]
#> 
#> ℹ Normalizing data again
#> ✔ Normalizing data again [7ms]
```

Yes, that’s it! Calling the magical
[`auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.md)
function will automatically perform the following steps, in the most
suitable way for your data:

- Normalization
- Missing value filtering
- Imputation
- Batch effect correction

and other steps that are necessary for downstream analysis.
