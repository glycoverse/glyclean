# auto_correct_batch_effect performs correction when threshold exceeded

    Code
      result <- auto_correct_batch_effect(exp, prop_threshold = 0.3)
    Message
      i Batch effects detected in 100.0% of variables (threshold: 30.0%). Performing batch correction.
      Found2batches
      Adjusting for1covariate(s) or covariate level(s)
      Standardizing Data across genes
      Fitting L/S model and finding priors
      Finding parametric adjustments
      Adjusting the Data
      
      v Batch correction completed.

# auto_correct_batch_effect skips correction when below threshold

    Code
      result <- auto_correct_batch_effect(exp, prop_threshold = 0.3)
    Message
      i Batch effects detected in 15.0% of variables (<= 30.0%). Skipping batch correction.

# auto_correct_batch_effect handles missing batch column

    Code
      result <- auto_correct_batch_effect(exp, batch_col = "batch")
    Message
      i Batch column 'batch' not found in sample_info. Skipping batch correction.

# auto_correct_batch_effect uses group information

    Code
      auto_correct_batch_effect(exp, group_col = "group", prop_threshold = 0)
    Message
      i Batch effects detected in 10.0% of variables (threshold: 0.0%). Performing batch correction.
      Found3batches
      Adjusting for1covariate(s) or covariate level(s)
      Standardizing Data across genes
      Fitting L/S model and finding priors
      Finding parametric adjustments
      Adjusting the Data
      
      v Batch correction completed.
      
      -- Others Experiment -----------------------------------------------------------
      i Expression matrix: 12 samples, 10 variables
      i Sample information fields: batch <chr>, group <chr>
      i Variable information fields: none

