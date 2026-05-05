# auto_clean works for glycoproteomics data

    Code
      result_exp <- auto_clean(test_exp, standardize_variable = FALSE)
    Message
      
      -- Normalizing data --
      
      i Using default normalization method for "glycoproteomics": `normalize_median()`.
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
      
      i Using default normalization method for "glycoproteomics": `normalize_median()`.
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works for glycoproteomics data with QC

    Code
      result_exp <- auto_clean(test_exp, standardize_variable = FALSE)
    Message
      
      -- Normalizing data --
      
      i Using default normalization method for "glycoproteomics": `normalize_median()`.
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
      
      i Using default normalization method for "glycoproteomics": `normalize_median()`.
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works with NULL qc_name

    Code
      result_exp <- auto_clean(test_exp, qc_name = NULL, standardize_variable = FALSE)
    Message
      
      -- Normalizing data --
      
      i Using default normalization method for "glycoproteomics": `normalize_median()`.
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
      
      i Using default normalization method for "glycoproteomics": `normalize_median()`.
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
      
      i Using default normalization method for "glycomics": `normalize_total_area()`.
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
      
      i Using default normalization method for "glycomics": `normalize_total_area()`.
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column  not found in sample_info. Skipping batch correction.
      v Batch correction completed.

