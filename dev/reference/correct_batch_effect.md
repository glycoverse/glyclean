# Correct Batch Effect

Correct batch effects in glycoproteomics/glycomics data using ComBat
algorithm from the sva package or removeBatchEffect from the limma
package.

## Usage

``` r
correct_batch_effect(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  method = c("combat", "limma")
)
```

## Arguments

- x:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  or
  [`SummarizedExperiment::SummarizedExperiment()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  object.

- batch:

  Either a factor/character vector specifying batch assignments for each
  sample, or a string specifying the column name in `sample_info`.
  Defaults to "batch".

- group:

  Either a factor/character vector specifying group assignments for each
  sample, or a string specifying the column name in `sample_info`. If
  provided, it will be used as a covariate in the batch correction
  model. This is useful when you have an unbalanced design. Default to
  NULL.

- check_confounding:

  Whether to check for confounding between batch and group variables.
  Default to TRUE.

- confounding_threshold:

  The threshold for Cramer's V to consider batch and group variables
  highly confounded. Only used when `check_confounding` is TRUE. Default
  to 0.4.

- method:

  The batch correction method to use. Either "combat" (default, uses
  sva::ComBat) or "limma" (uses limma::removeBatchEffect). Default to
  "combat".

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with batch effects corrected. SummarizedExperiment inputs return
the same class.

## Details

This function performs batch effect correction using either the ComBat
algorithm or the limma removeBatchEffect function. It requires batch
information provided via the `batch` parameter. If no batch information
is available, the function will return the original data unchanged.

If group information is provided via `group`, the function will check
for confounding between batch and group variables. If batch and group
are highly confounded (\|Cramer's V\| \> `threshold`), the function will
issue a warning and return the original data unchanged to avoid
over-correction.

When both batch and group information are available and not highly
confounded, the group information will be included in the model to
preserve biological variation while correcting for batch effects.

## Examples

``` r
library(SummarizedExperiment)
#> Loading required package: MatrixGenerics
#> Loading required package: matrixStats
#> 
#> Attaching package: ‘MatrixGenerics’
#> The following objects are masked from ‘package:matrixStats’:
#> 
#>     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
#>     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
#>     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
#>     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
#>     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
#>     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
#>     colWeightedMeans, colWeightedMedians, colWeightedSds,
#>     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
#>     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
#>     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
#>     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
#>     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
#>     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
#>     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
#>     rowWeightedSds, rowWeightedVars
#> Loading required package: GenomicRanges
#> Loading required package: stats4
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> Loading required package: S4Vectors
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
#> Loading required package: IRanges
#> Loading required package: Seqinfo
#> Loading required package: Biobase
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> 
#> Attaching package: ‘Biobase’
#> The following object is masked from ‘package:MatrixGenerics’:
#> 
#>     rowMedians
#> The following objects are masked from ‘package:matrixStats’:
#> 
#>     anyMissing, rowMedians
#> The following object is masked from ‘package:glyexp’:
#> 
#>     samples

# With a SummarizedExperiment and column names
exp <- glyexp::real_experiment
batch <- rep(c("A", "B"), length.out = ncol(exp))
group <- rep(c("Ctrl", "Ctrl", "Treat", "Treat"), length.out = ncol(exp))
if (inherits(exp, "glyexp_experiment")) {
  exp$sample_info$batch <- batch
  exp$sample_info$group <- group
} else {
  SummarizedExperiment::colData(exp)$batch <- batch
  SummarizedExperiment::colData(exp)$group <- group
}
corrected_exp <- correct_batch_effect(exp, batch = "batch", group = "group")
#> Found2batches
#> Adjusting for1covariate(s) or covariate level(s)
#> Found4063Missing Data Values 
#> Standardizing Data across genes
#> Warning: ComBat failed to correct batch effects.
#> ℹ Error: Lapack routine dgesv: system is exactly singular: U[3,3] = 0
#> ℹ Returning original data unchanged.

# Using limma method
corrected_exp <- correct_batch_effect(
  exp, batch = "batch", group = "group", method = "limma"
)
#> design matrix of interest not specified. Assuming a one-group experiment.
#> Coefficients not estimable: (Intercept) 
```
