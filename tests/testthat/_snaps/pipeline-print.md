# pipeline print uses cli headings, lists and wrapped parameter text

    Code
      cat(capture_pipeline_print(pipeline, width = 60)$text, "\n", sep = "")
    Message
      
      -- <glyclean_pipeline> 2 steps -----------------------------
      1. normalize_total_area
         (no parameters)
      2. impute_min_prob
         seed = 42, by = NULL, q = 0.01, tune.sigma = 1
    Output
      

