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
      * Raw data: Median CV = CV_VALUE
      * Method "normalize_median": Median CV = CV_VALUE
      * Method "normalize_median_abs": Median CV = CV_VALUE
      * Method "normalize_total_area": Median CV = CV_VALUE
      * Method "normalize_quantile": Median CV = CV_VALUE
      * Method "normalize_median_quotient": Median CV = CV_VALUE
      * Method "normalize_loessf": Median CV = CV_VALUE
      * Method "normalize_loesscyc": Median CV = CV_VALUE
      * Method "normalize_rlr": Median CV = CV_VALUE
      * Method "normalize_rlrma": Median CV = CV_VALUE
      * Method "normalize_rlrmacyc": Median CV = CV_VALUE
      v Best method: "BEST_METHOD" with Median CV = CV_VALUE
      
      -- Removing variables with too many missing values --
      
      QC samples found. Excluding 2 QC samples from removal process.
      Applying preset "discovery"...
      No variables removed.
      
      -- Imputing missing values --
      
      QC samples found. Choosing the best imputation method based on QC samples.
      * Raw data: Median CV = CV_VALUE
      * Method "impute_zero": Median CV = CV_VALUE
      * Method "impute_sample_min": Median CV = CV_VALUE
      * Method "impute_half_sample_min": Median CV = CV_VALUE
      * Method "impute_sw_knn": Median CV = CV_VALUE
      * Method "impute_fw_knn": Median CV = CV_VALUE
      * Method "impute_bpca": Median CV = CV_VALUE
      * Method "impute_ppca": Median CV = CV_VALUE
      * Method "impute_svd": Median CV = CV_VALUE
      * Method "impute_min_prob": Median CV = CV_VALUE
      * Method "impute_miss_forest": Median CV = CV_VALUE
      v Best method: "BEST_METHOD" with Median CV = CV_VALUE
      
      -- Aggregating data --
      
      Aggregating to "gfs" level
      
      -- Normalizing data again --
      
      QC samples found. Choosing the best normalization method based on QC samples.
      * Raw data: Median CV = CV_VALUE
      * Method "normalize_median": Median CV = CV_VALUE
      * Method "normalize_median_abs": Median CV = CV_VALUE
      * Method "normalize_total_area": Median CV = CV_VALUE
      * Method "normalize_quantile": Median CV = CV_VALUE
      * Method "normalize_median_quotient": Median CV = CV_VALUE
      * Method "normalize_loessf": Median CV = 0
      * Method "normalize_loesscyc": Median CV = 0
      * Method "normalize_rlr": Median CV = CV_VALUE
      * Method "normalize_rlrma": Median CV = CV_VALUE
      * Method "normalize_rlrmacyc": Median CV = CV_VALUE
      v Best method: "BEST_METHOD" with Median CV = 0
      
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
      * Raw data: Median CV = CV_VALUE
      * Method "normalize_median": Median CV = CV_VALUE
      * Method "normalize_median_abs": Median CV = CV_VALUE
      * Method "normalize_total_area": Median CV = CV_VALUE
      * Method "normalize_quantile": Median CV = CV_VALUE
      * Method "normalize_median_quotient": Median CV = CV_VALUE
      * Method "normalize_loessf": Median CV = CV_VALUE
      * Method "normalize_loesscyc": Median CV = CV_VALUE
      * Method "normalize_rlr": Median CV = CV_VALUE
      * Method "normalize_rlrma": Median CV = CV_VALUE
      * Method "normalize_rlrmacyc": Median CV = CV_VALUE
      v Best method: "BEST_METHOD" with Median CV = CV_VALUE
      
      -- Normalizing data (Total Area) --
      
      -- Imputing missing values --
      
      QC samples found. Choosing the best imputation method based on QC samples.
      * Raw data: Median CV = CV_VALUE
      * Method "impute_zero": Median CV = CV_VALUE
      * Method "impute_sample_min": Median CV = CV_VALUE
      * Method "impute_half_sample_min": Median CV = CV_VALUE
      * Method "impute_sw_knn": Median CV = CV_VALUE
      * Method "impute_fw_knn": Median CV = CV_VALUE
      * Method "impute_bpca": Median CV = CV_VALUE
      * Method "impute_ppca": Median CV = CV_VALUE
      * Method "impute_svd": Median CV = CV_VALUE
      * Method "impute_min_prob": Median CV = CV_VALUE
      * Method "impute_miss_forest": Median CV = CV_VALUE
      v Best method: "BEST_METHOD" with Median CV = CV_VALUE
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.

