# Select geometric median as reference for ALR

The geometric median is the variable (row) that has the smallest sum of
squared log-ratio distances to all other variables. This provides the
most stable reference for ALR transformation.

## Usage

``` r
.select_geometric_median(mat)
```

## Arguments

- mat:

  A matrix with variables as rows and samples as columns.

## Value

The index of the geometric median variable.
