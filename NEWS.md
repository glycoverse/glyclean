# glyclean 0.4.0

## Major changes

- All data processing functions now accept matrices as input in addition to `glyexp_experiment` objects.
- The `by` parameter in data processing functions now accepts factors in addition to column names, 
  enabling direct use with matrix inputs.
- API updated to support custom grouping factors for batch operations when working with matrices.

## Minor improvements

- Improved error messages for `correct_batch_effect()` and `detect_batch_effect()` when column 
  names are not found in sample information.
- More meaningful error messages when providing column names as `batch` or `group` arguments 
  with matrix inputs.
- Enhanced parameter validation and error handling across all modules.

## Bug fixes

- Fixed incorrect behavior of `by` argument in `filter_missing_variable()`.
- Resolved issues with parameter processing when using different input types.

# glyclean 0.3.0

- `auto_clean()` now detects batch effects before batch correction.
- `to_level` argument is removed from `auto_clean()`.