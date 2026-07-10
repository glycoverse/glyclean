# auto_impute uses deterministic defaults with QC samples

    Code
      imputed <- auto_impute(exp, group_col = "group")
    Message
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with 30 <= n_samples <= 100.

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

# auto_impute validates input

    Code
      result <- auto_impute(exp)
    Message
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with n_samples < 30.

