# auto_aggregate works for glycoproteomics experiments

    Code
      result_exp <- auto_aggregate(exp)
    Message
      Aggregating to "gfs" level

# auto_aggregate rejects non-glycoproteomics experiments

    Code
      auto_aggregate(exp)
    Condition
      Error in `auto_aggregate()`:
      ! The experiment type must be "glycoproteomics".
      x Got "glycomics".

# auto_aggregate works for experiments without glycan structure column

    Code
      result_exp <- auto_aggregate(exp)
    Message
      Aggregating to "gf" level

