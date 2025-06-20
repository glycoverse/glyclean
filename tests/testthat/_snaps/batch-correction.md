# correct_batch_effect returns original experiment when no batch info

    Code
      result <- correct_batch_effect(exp)
    Message
      i No batch information found in column 'batch' of sample_info. Returning original experiment unchanged.

# correct_batch_effect warns and returns original when batch and group are confounded

    Code
      result <- correct_batch_effect(exp, group_col = "group")
    Condition
      Warning:
      Batch and group variables are highly confounded.
      i Each batch contains only one group, making batch correction problematic.
      i Returning original experiment unchanged to avoid over-correction.

# correct_batch_effect handles insufficient samples per batch

    Code
      result <- correct_batch_effect(exp)
    Condition
      Warning:
      Some batches have fewer than 2 samples.
      i ComBat requires at least 2 samples per batch.
      i Returning original experiment unchanged.

