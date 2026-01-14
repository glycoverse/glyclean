# auto_clean works for glycoproteomics data

    Code
      result_exp <- auto_clean(test_exp, standardize_variable = FALSE)
    Message
      
      -- Normalizing data --
      
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "glycoproteomics". Using `normalize_median()`.
      v Normalization completed.
      
      -- Removing variables with too many missing values --
      
      i No QC samples found. Using all samples.
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Imputing missing values --
      
      i No QC samples found. Using default imputation method based on sample size.
      i Sample size <= 30, using `impute_sample_min()`.
      v Imputation completed.
      
      -- Aggregating data --
      
      i Aggregating to "gfs" level
      v Aggregation completed.
      
      -- Normalizing data again --
      
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "glycoproteomics". Using `normalize_median()`.
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works for glycoproteomics data with QC

    Code
      result_exp <- auto_clean(test_exp, standardize_variable = FALSE)
    Message
      
      -- Normalizing data --
      
      i QC samples found. Choosing the best normalization method based on QC samples.
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
      v Normalization completed.
      
      -- Removing variables with too many missing values --
      
      i QC samples found. Excluding 2 QC samples from removal process.
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Imputing missing values --
      
      i QC samples found. Choosing the best imputation method based on QC samples.
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
      v Imputation completed.
      
      -- Aggregating data --
      
      i Aggregating to "gfs" level
      v Aggregation completed.
      
      -- Normalizing data again --
      
      i QC samples found. Choosing the best normalization method based on QC samples.
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
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works with NULL qc_name

    Code
      result_exp <- auto_clean(test_exp, qc_name = NULL, standardize_variable = FALSE)
    Message
      
      -- Normalizing data --
      
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "glycoproteomics". Using `normalize_median()`.
      v Normalization completed.
      
      -- Removing variables with too many missing values --
      
      i No QC samples found. Using all samples.
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Imputing missing values --
      
      i No QC samples found. Using default imputation method based on sample size.
      i Sample size <= 30, using `impute_sample_min()`.
      v Imputation completed.
      
      -- Aggregating data --
      
      i Aggregating to "gfs" level
      v Aggregation completed.
      
      -- Normalizing data again --
      
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "glycoproteomics". Using `normalize_median()`.
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works for glycomics data

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Removing variables with too many missing values --
      
      i No QC samples found. Using all samples.
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Normalizing data --
      
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "glycomics". Using `normalize_median_quotient()` + `normalize_total_area()`.
      v Normalization completed.
      
      -- Normalizing data (Total Area) --
      
      v Total area normalization completed.
      
      -- Imputing missing values --
      
      i No QC samples found. Using default imputation method based on sample size.
      i Sample size <= 30, using `impute_sample_min()`.
      v Imputation completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works for glycomics data with QC

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Removing variables with too many missing values --
      
      i QC samples found. Excluding 2 QC samples from removal process.
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Normalizing data --
      
      i QC samples found. Choosing the best normalization method based on QC samples.
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
      v Normalization completed.
      
      -- Normalizing data (Total Area) --
      
      v Total area normalization completed.
      
      -- Imputing missing values --
      
      i QC samples found. Choosing the best imputation method based on QC samples.
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
      v Imputation completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

