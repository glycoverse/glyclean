# Resolve column specification to values

This function handles both column names (strings) and direct
factor/vector inputs.

## Usage

``` r
.resolve_column_param(
  param,
  sample_info = NULL,
  param_name = "parameter",
  n_samples = NULL,
  allow_null = TRUE
)
```

## Arguments

- param:

  The parameter to resolve (can be string, factor, or vector)

- sample_info:

  The sample_info data frame (for experiment objects)

- param_name:

  The name of the parameter (for error messages)

- n_samples:

  The number of samples (for validation)

- allow_null:

  Whether NULL values are allowed

## Value

The resolved values as a vector, or NULL if param is NULL and allow_null
is TRUE
