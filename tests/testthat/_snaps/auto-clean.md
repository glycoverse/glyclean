# auto_clean works for glycoproteomics data

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Normalizing data --
      
      No QC samples found. Using default normalization method based on experiment type.
      Experiment type is "glycoproteomics". Using `normalize_median()`.
      
      -- Removing variables with too many missing values --
      
      No QC samples found. Using all samples.
      Applying preset "discovery"...
      No variables removed.
      
      -- Imputing missing values --
      
      No QC samples found. Using default imputation method based on sample size.
      Sample size <= 30, using `impute_sample_min()`.
      
      -- Aggregating data --
      
      Aggregating to "gfs" level
      
      -- Normalizing data again --
      
      No QC samples found. Using default normalization method based on experiment type.
      Experiment type is "glycoproteomics". Using `normalize_median()`.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.

# auto_clean works for glycoproteomics data with QC

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Normalizing data --
      
      QC samples found. Choosing the best normalization method based on QC samples.
      Median CV of raw data: 0.0204
      * Method "normalize_median": Median CV = 0.02768
      * Method "normalize_median_abs": Median CV = 0.02768
      * Method "normalize_total_area": Median CV = 0.02099
      * Method "normalize_quantile": Median CV = 0.02972
      * Method "normalize_median_quotient": Median CV = 0.03102
      * Method "normalize_loessf": Median CV = 0.02155
      * Method "normalize_loesscyc": Median CV = 0.03793
      * Method "normalize_rlr": Median CV = 0.1593
      * Method "normalize_rlrma": Median CV = 0.01822
      * Method "normalize_rlrmacyc": Median CV = 0.04225
      v Best method: "normalize_rlrma" with Median CV = 0.01822
      
      -- Removing variables with too many missing values --
      
      QC samples found. Excluding 2 QC samples from removal process.
      Applying preset "discovery"...
      No variables removed.
      
      -- Imputing missing values --
      
      QC samples found. Choosing the best imputation method based on QC samples.
      Median CV of raw data (excluding NA): 0.01822
      * Method "impute_zero": Median CV = 0.01822
      * Method "impute_sample_min": Median CV = 0.01822
      * Method "impute_half_sample_min": Median CV = 0.01822
      * Method "impute_sw_knn": Median CV = 0.01822
      * Method "impute_fw_knn": Median CV = 0.01822
      * Method "impute_bpca": Median CV = 0.01822
      * Method "impute_ppca": Median CV = 0.01822
      * Method "impute_svd": Median CV = 0.01822
      * Method "impute_min_prob": Median CV = 0.01822
      * Method "impute_miss_forest": Median CV = 0.01822
      v Best method: "impute_zero" with Median CV = 0.01822
      
      -- Aggregating data --
      
      Aggregating to "gfs" level
      
      -- Normalizing data again --
      
      QC samples found. Choosing the best normalization method based on QC samples.
      Median CV of raw data: 0.03443
      * Method "normalize_median": Median CV = 0.04429
      * Method "normalize_median_abs": Median CV = 0.04429
      * Method "normalize_total_area": Median CV = 0.02868
      * Method "normalize_quantile": Median CV = 0.01818
      * Method "normalize_median_quotient": Median CV = 0.05719
      * Method "normalize_loessf": Median CV = 0
      * Method "normalize_loesscyc": Median CV = 0
      * Method "normalize_rlr": Median CV = 0.05383
      * Method "normalize_rlrma": Median CV = 0.05452
      * Method "normalize_rlrmacyc": Median CV = 0.06133
      v Best method: "normalize_loessf" with Median CV = 0
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.

# auto_clean works for glycomics data

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Removing variables with too many missing values --
      
      No QC samples found. Using all samples.
      Applying preset "discovery"...
      No variables removed.
      
      -- Normalizing data --
      
      No QC samples found. Using default normalization method based on experiment type.
      Experiment type is "glycomics". Using `normalize_median_quotient()` + `normalize_total_area()`.
      
      -- Normalizing data (Total Area) --
      
      -- Imputing missing values --
      
      No QC samples found. Using default imputation method based on sample size.
      Sample size <= 30, using `impute_sample_min()`.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.

# auto_clean works for glycomics data with QC

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Removing variables with too many missing values --
      
      QC samples found. Excluding 2 QC samples from removal process.
      Applying preset "discovery"...
      No variables removed.
      
      -- Normalizing data --
      
      QC samples found. Choosing the best normalization method based on QC samples.
      Median CV of raw data: 0.7512
      * Method "normalize_median": Median CV = 0.5823
      * Method "normalize_median_abs": Median CV = 0.5823
      * Method "normalize_total_area": Median CV = 0.6239
      * Method "normalize_quantile": Median CV = 0.5569
      * Method "normalize_median_quotient": Median CV = 0.4857
      * Method "normalize_loessf": Median CV = 0.058
      * Method "normalize_loesscyc": Median CV = 0.0105
      * Method "normalize_rlr": Median CV = 1.035
      * Method "normalize_rlrma": Median CV = 0.6126
      * Method "normalize_rlrmacyc": Median CV = 0.7607
      v Best method: "normalize_loesscyc" with Median CV = 0.0105
      
      -- Normalizing data (Total Area) --
      
      -- Imputing missing values --
      
      QC samples found. Choosing the best imputation method based on QC samples.
      Median CV of raw data (excluding NA): 0.01094
      * Method "impute_zero": Median CV = 0.01094
      * Method "impute_sample_min": Median CV = 0.01094
      * Method "impute_half_sample_min": Median CV = 0.01094
      * Method "impute_sw_knn": Median CV = 0.01094
      * Method "impute_fw_knn": Median CV = 0.01094
      * Method "impute_bpca": Median CV = 0.01094
    Condition
      Warning in `ppca()`:
      stopped after max iterations, but rel_ch was > threshold
    Message
      * Method "impute_ppca": Median CV = 0.01094
      * Method "impute_svd": Median CV = 0.01094
      * Method "impute_min_prob": Median CV = 0.01094
      * Method "impute_miss_forest": Median CV = 0.01094
      v Best method: "impute_zero" with Median CV = 0.01094
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.

