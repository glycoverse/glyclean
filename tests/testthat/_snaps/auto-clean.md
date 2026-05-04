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
      
      i Using default imputation method for "glycoproteomics" with n_samples < 30: `impute_min_prob()`.
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
      
      i Using default imputation method for "glycoproteomics" with n_samples < 30: `impute_min_prob()`.
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
      * Method "normalize_loessf": Median CV = CV_VALUE
      * Method "normalize_loesscyc": Median CV = CV_VALUE
      * Method "normalize_rlr": Median CV = CV_VALUE
      * Method "normalize_rlrma": Median CV = CV_VALUE
      * Method "normalize_rlrmacyc": Median CV = CV_VALUE
      v Best method: "BEST_METHOD" with Median CV = CV_VALUE
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
      
      i Using default imputation method for "glycoproteomics" with n_samples < 30: `impute_min_prob()`.
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
      
      -- Imputing missing values --
      
      i Using default imputation method for "glycomics" with n_samples < 30: `impute_min_prob()`.
      v Imputation completed.
      
      -- Normalizing data --
      
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "glycomics" with "nrow(exp)" glycans.
      v Normalization completed.
      
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
      
      -- Imputing missing values --
      
      i Using default imputation method for "glycomics" with n_samples < 30: `impute_min_prob()`.
      v Imputation completed.
      
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
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

