# glyclean 0.6.2

## Bug fixes

- Add `scales` to `Imports`.

# glyclean 0.6.1

## Minor improvements

- Update the documentation of `aggregate()`.

## Bug fixes

- Fix a bug in `aggregate()` where it falled on experiments returned by most `glyread` functions.

# glyclean 0.6.0

## Major changes

- Add `add_site_seq()` function to add site sequences to a glycopeptide experiment.

# glyclean 0.5.0

## Major changes

- Add `infer_protein()` function to resolve multiple protein assignments for glycopeptides.
- Add `adjust_protein()` function to remove protein expression from glycopeptide expression.

## Minor improvements

- Rename the first argument of `aggregate()` from `x` to `exp` to be consistent with other functions.

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
