# auto_impute uses deterministic defaults with QC samples

    Code
      imputed <- auto_impute(exp, group_col = "group", qc_name = "QC", to_try = list(
        impute_sample_min = function(x) stop("must not be called")))
    Message
      i QC samples found. Using deterministic default imputation method.
      i Using default imputation method for "glycoproteomics" with 30 <= n_samples <= 100: `impute_min_prob()`.

# auto_impute handles NULL qc_name

    Code
      imputed <- auto_impute(exp, group_col = "group", qc_name = NULL)
    Message
      i No QC samples found. Using deterministic default imputation method.
      i Using default imputation method for "glycomics" with n_samples < 30: `impute_min_prob()`.

# auto_impute handles missing group_col gracefully

    Code
      result <- auto_impute(exp, group_col = NULL)
    Message
      i No QC samples found. Using deterministic default imputation method.
      i Using default imputation method for "glycoproteomics" with n_samples < 30: `impute_min_prob()`.

# auto_impute handles non-existent group_col gracefully

    Code
      result <- auto_impute(exp, group_col = "nonexistent")
    Message
      i No QC samples found. Using deterministic default imputation method.
      i Using default imputation method for "glycoproteomics" with n_samples < 30: `impute_min_prob()`.

