# auto_impute works with QC samples

    Code
      imputed <- auto_impute(exp, group_col = "group", qc_name = "QC")
    Message
      QC samples found. Choosing the best imputation method based on QC samples.
      * Raw data: Median CV = 0.07814
      * Method "impute_zero": Median CV = 0.07814
      * Method "impute_sample_min": Median CV = 0.07814
      * Method "impute_half_sample_min": Median CV = 0.07814
      * Method "impute_sw_knn": Median CV = 0.07814
      * Method "impute_fw_knn": Median CV = 0.07814
      * Method "impute_bpca": Median CV = 0.07814
      * Method "impute_ppca": Median CV = 0.07814
      * Method "impute_svd": Median CV = 0.07814
      * Method "impute_min_prob": Median CV = 0.07814
      * Method "impute_miss_forest": Median CV = 0.07814
      v Best method: "impute_zero" with Median CV = 0.07814

# auto_impute uses sample_min for small datasets without QC

    Code
      auto <- auto_impute(exp, group_col = NULL)
    Message
      No QC samples found. Using default imputation method based on sample size.
      Sample size <= 30, using `impute_sample_min()`.

# auto_impute uses min_prob for medium datasets without QC

    Code
      auto <- auto_impute(exp, group_col = NULL)
    Message
      No QC samples found. Using default imputation method based on sample size.
      Sample size <= 100, using `impute_min_prob()`.

# auto_impute handles missing group_col gracefully

    Code
      result <- auto_impute(exp, group_col = NULL)
    Message
      No QC samples found. Using default imputation method based on sample size.
      Sample size <= 30, using `impute_sample_min()`.

# auto_impute handles non-existent group_col gracefully

    Code
      result <- auto_impute(exp, group_col = "nonexistent")
    Message
      No QC samples found. Using default imputation method based on sample size.
      Sample size <= 30, using `impute_sample_min()`.

# auto_impute works with custom methods

    Code
      result <- auto_impute(exp, to_try = custom_methods)
    Message
      QC samples found. Choosing the best imputation method based on QC samples.
      * Raw data: Median CV = 0.07814
      * Method "impute_zero": Median CV = 0.07814
      * Method "impute_sample_min": Median CV = 0.07814
      v Best method: "impute_zero" with Median CV = 0.07814

