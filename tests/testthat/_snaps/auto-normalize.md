# auto_normalize uses deterministic defaults with QC samples

    Code
      normed <- auto_normalize(exp, group_col = "group")
    Message
      i Normalization method: `normalize_total_area()`
      i Reason: default for "glycomics".

# auto_normalize works for glycoproteomics without QC

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      i Normalization method: `normalize_median()`
      i Reason: default for "glycoproteomics".

# auto_normalize falls back to median for others

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      i Normalization method: `normalize_median()`
      i Reason: default for "others".

