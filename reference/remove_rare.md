# Remove Rare Variables with Too Many Missing Values

Remove Rare Variables with Too Many Missing Values

## Usage

``` r
remove_rare(x, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL)

# S3 method for class 'glyexp_experiment'
remove_rare(x, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL)

# S3 method for class 'matrix'
remove_rare(x, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL)

# Default S3 method
remove_rare(x, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

- prop:

  The proportion of missing values to use as a threshold. Variables with
  missing values above this threshold will be removed. Defaults to 0.5.

- n:

  The number of missing values to use as a threshold. An alternative to
  `prop`.

- by:

  Either a column name in `sample_info` (string) or a factor/vector
  specifying group assignments for each sample. Missing value counts or
  proportions will be calculated within each group.

- strict:

  Works with `by`. If `FALSE`, remove a variable only if it passes the
  missing threshold in all groups. If `TRUE`, remove a variable if it
  passes the missing threshold in any group. See examples for more
  details.

- min_n:

  The minimum number of non-missing values required for a variable to be
  kept. If `NULL` (default), it is calculated dynamically:

  - For datasets with 1, 2, or 3 samples: min_n equals the sample count

  - For datasets with \>3 samples: min_n = 3

  - When using `by`, the rule is applied within each group

## Value

For `glyexp_experiment` input, returns a modified `glyexp_experiment`
object. For matrix input, returns a filtered matrix.

## Examples

``` r
# With glyexp_experiment
exp <- glyexp::toy_experiment
exp$expr_mat[1, 1] <- NA    # V1: 1/6 missing
exp$expr_mat[2, 1:3] <- NA  # V2: 3/6 missing
exp$expr_mat[3, 1:5] <- NA  # V3: 5/6 missing
exp$expr_mat[4, 1:6] <- NA  # V4: 6/6 missing
exp$expr_mat
#>    S1 S2 S3 S4 S5 S6
#> V1 NA  5  9 13 17 21
#> V2 NA NA NA 14 18 22
#> V3 NA NA NA NA NA 23
#> V4 NA NA NA NA NA NA

# Remove variables with more than 50% missing values.
remove_rare(exp, prop = 0.5)$expr_mat
#> ℹ Removed 2 of 4 (50%) variables.
#>    S1 S2 S3 S4 S5 S6
#> V1 NA  5  9 13 17 21
#> V2 NA NA NA 14 18 22

# Remove variables with more than 2 missing values.
remove_rare(exp, n = 2)$expr_mat
#> ℹ Removed 3 of 4 (75%) variables.
#>    S1 S2 S3 S4 S5 S6
#> V1 NA  5  9 13 17 21

# Remove variables if they have more than 1 missing value in all groups.
# In another word, keep variables as long as they have 1 or 0 missing value
# in any group.
remove_rare(exp, by = "group", strict = FALSE)$expr_mat
#> ℹ Removed 2 of 4 (50%) variables.
#>    S1 S2 S3 S4 S5 S6
#> V1 NA  5  9 13 17 21
#> V2 NA NA NA 14 18 22

# Keep only variables with no missing values.
remove_rare(exp, prop = 0)$expr_mat
#> ℹ Removed 4 of 4 (100%) variables.
#>      S1 S2 S3 S4 S5 S6

# Use custom min_n to require at least 4 non-missing values
remove_rare(exp, min_n = 4)$expr_mat
#> ℹ Removed 3 of 4 (75%) variables.
#>    S1 S2 S3 S4 S5 S6
#> V1 NA  5  9 13 17 21

# With matrix
mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3)
mat_filtered <- remove_rare(mat, prop = 0.5)
#> ℹ Removed 1 of 3 (33.33%) variables.
```
