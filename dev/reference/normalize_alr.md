# Additive Log-Ratio Normalization

This function performs Additive Log-Ratio (ALR) transformation on
compositional data. ALR transforms each component by taking the
logarithm of the ratio of the component to a reference component. This
is useful for analyzing compositional data where the relative
proportions are of interest.

## Usage

``` r
normalize_alr(x)

# S3 method for class 'glyexp_experiment'
normalize_alr(x)

# S3 method for class 'matrix'
normalize_alr(x)

# Default S3 method
normalize_alr(x)
```

## Arguments

- x:

  Either a `glyexp_experiment` object or a matrix. If a matrix, rows
  should be variables and columns should be samples.

## Value

Returns the same type as the input. If `x` is a `glyexp_experiment`,
returns a `glyexp_experiment` with an ALR-transformed expression matrix.
If `x` is a matrix, returns an ALR-transformed matrix. In both cases,
the returned matrix has the same number of rows (variables) as the
input: the automatically selected reference variable is retained as a
row of zeros after transformation, and all other variables contain
log-ratios relative to this reference. The reference variable is the
geometric-median variable described above, and its row position/name is
preserved. Note that the resulting values are on the log scale and can
be negative.

## Details

The formula is: `alr(x)_i = log(x_i / x_ref)` where `x_ref` is the
reference component.

This function requires all values to be strictly positive (no zeros or
NA values). Users must handle zeros and missing values before calling
this function.

The reference variable is automatically selected as the geometric median
(the variable with the smallest sum of log-ratio distances to all other
variables), which provides the most stable reference.

This function is a wrapper around
[`compositions::alr()`](https://rdrr.io/pkg/compositions/man/alr.html).
