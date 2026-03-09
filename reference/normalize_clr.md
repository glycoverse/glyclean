# Centered Log-Ratio Normalization

This function performs Centered Log-Ratio (CLR) transformation on
compositional data. CLR transforms each component by taking the
logarithm of the ratio of the component to the geometric mean of all
components. This is useful for analyzing compositional data where the
relative proportions are of interest rather than absolute values.

## Usage

``` r
normalize_clr(x)

# S3 method for class 'glyexp_experiment'
normalize_clr(x)

# S3 method for class 'matrix'
normalize_clr(x)

# Default S3 method
normalize_clr(x)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with CLR-transformed expression matrix. If
`x` is a matrix, returns a CLR-transformed matrix. Note that the
resulting values are on the log scale and can be negative.

## Details

The formula is: `clr(x)_i = log(x_i / g(x))` where `g(x)` is the
geometric mean of all components in the sample.

This function requires all values to be strictly positive (no zeros or
NA values). Users must handle zeros and missing values before calling
this function.

This function is a wrapper around
[`compositions::clr()`](https://rdrr.io/pkg/compositions/man/clr.html).
