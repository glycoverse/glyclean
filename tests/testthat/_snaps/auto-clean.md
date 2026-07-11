# auto_clean works for glycoproteomics data

    Code
      result_exp <- auto_clean(test_exp, standardize_variable = FALSE)
    Message
      
      -- Removing variables with too many missing values --
      
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Normalizing data --
      
      i Normalization method: `normalize_median()`
      i Reason: default for "glycoproteomics".
      v Normalization completed.
      
      -- Imputing missing values --
      
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with n_samples < 30.
      v Imputation completed.
      
      -- Aggregating data --
      
      i Aggregating to "gfs" level
      v Aggregation completed.
      
      -- Normalizing data again --
      
      i Normalization method: `normalize_median()`
      i Reason: default for "glycoproteomics".
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column batch not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works for glycoproteomics data with QC

    Code
      result_exp <- auto_clean(test_exp, standardize_variable = FALSE)
    Message
      
      -- Removing variables with too many missing values --
      
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Normalizing data --
      
      i Normalization method: `normalize_median()`
      i Reason: default for "glycoproteomics".
      v Normalization completed.
      
      -- Imputing missing values --
      
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycoproteomics" with n_samples < 30.
      v Imputation completed.
      
      -- Aggregating data --
      
      i Aggregating to "gfs" level
      v Aggregation completed.
      
      -- Normalizing data again --
      
      i Normalization method: `normalize_median()`
      i Reason: default for "glycoproteomics".
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column batch not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works for glycomics data

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Removing variables with too many missing values --
      
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Imputing missing values --
      
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycomics" with n_samples < 30.
      v Imputation completed.
      
      -- Normalizing data --
      
      i Normalization method: `normalize_total_area()`
      i Reason: default for "glycomics".
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column batch not found in sample_info. Skipping batch correction.
      v Batch correction completed.

# auto_clean works for glycomics data with QC

    Code
      result_exp <- auto_clean(test_exp)
    Message
      
      -- Removing variables with too many missing values --
      
      i Applying preset "discovery"...
      i No variables removed.
      v Variable removal completed.
      
      -- Imputing missing values --
      
      i Imputation method: `impute_min_prob()`
      i Reason: default for "glycomics" with n_samples < 30.
      v Imputation completed.
      
      -- Normalizing data --
      
      i Normalization method: `normalize_total_area()`
      i Reason: default for "glycomics".
      v Normalization completed.
      
      -- Correcting batch effects --
      
      i Batch column batch not found in sample_info. Skipping batch correction.
      v Batch correction completed.

