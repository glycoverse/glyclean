# auto_normalize uses deterministic defaults with QC samples

    Code
      normed <- auto_normalize(exp, group_col = "group", qc_name = "QC", to_try = list(
        normalize_median = function(x) stop("must not be called")))
    Condition
      Warning:
      The `to_try` argument of `auto_normalize()` is deprecated as of glyclean 0.14.0.
      i The automatic normalization strategy is now deterministic and does not require user-specified methods to try. The `to_try` parameter will be removed in a future release.
    Message
      i Normalization method: `normalize_total_area()`
      i Reason: default for "glycomics".

# auto_normalize handles NULL qc_name

    Code
      normed <- auto_normalize(exp, group_col = "group", qc_name = NULL)
    Condition
      Warning:
      The `qc_name` argument of `auto_normalize()` is deprecated as of glyclean 0.14.0.
      i This function no longer uses the `qc_name` parameter and it will be removed in a future release.
    Message
      i Normalization method: `normalize_total_area()`
      i Reason: default for "glycomics".

# auto_normalize works for glycoproteomics without QC

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      i Normalization method: `normalize_median()`
      i Reason: default for "glycoproteomics".

# auto_normalize falls back to median for others

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      i Normalization method: `normalize_median()`
      i Reason: default for "others".

