# auto_impute uses deterministic defaults with QC samples

    Code
      imputed <- auto_impute(exp, group_col = "group", qc_name = "QC", to_try = list(
        impute_sample_min = function(x) stop("must not be called")))
    Condition
      Warning:
      The `to_try` argument of `auto_impute()` is deprecated as of glyclean 0.14.0.
      i The automatic imputation strategy is now deterministic and does not require user-specified methods to try. The `to_try` parameter will be removed in a future release.
    Message
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with 30 <= n_samples <= 100.

# auto_impute handles NULL qc_name

    Code
      imputed <- auto_impute(exp, group_col = "group", qc_name = NULL)
    Condition
      Warning:
      The `qc_name` argument of `auto_impute()` is deprecated as of glyclean 0.14.0.
      i This function no longer uses QC sample information and the `qc_name` parameter will be removed in a future release.
    Message
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycomics" with n_samples < 30.

# auto_impute rejects unsupported experiment types

    Code
      auto_impute(exp, group_col = NULL)
    Condition
      Error in `.choose_auto_impute_strategy()`:
      ! Can only apply automatic imputation on glycomics and glycoproteomics experiments.
      x Experiment type "traitomics" is not supported.

---

    Code
      auto_impute(exp, group_col = NULL)
    Condition
      Error in `.choose_auto_impute_strategy()`:
      ! Can only apply automatic imputation on glycomics and glycoproteomics experiments.
      x Experiment type "traitproteomics" is not supported.

# auto_impute handles missing group_col gracefully

    Code
      result <- auto_impute(exp, group_col = NULL)
    Message
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with n_samples < 30.

# auto_impute handles non-existent group_col gracefully

    Code
      result <- auto_impute(exp, group_col = "nonexistent")
    Message
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with n_samples < 30.

# auto_impute validates input and ignores deprecated arguments

    Code
      result <- auto_impute(exp, qc_name = NULL, to_try = "not_a_list")
    Condition
      Warning:
      The `qc_name` argument of `auto_impute()` is deprecated as of glyclean 0.14.0.
      i This function no longer uses QC sample information and the `qc_name` parameter will be removed in a future release.
      Warning:
      The `to_try` argument of `auto_impute()` is deprecated as of glyclean 0.14.0.
      i The automatic imputation strategy is now deterministic and does not require user-specified methods to try. The `to_try` parameter will be removed in a future release.
    Message
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with n_samples < 30.

