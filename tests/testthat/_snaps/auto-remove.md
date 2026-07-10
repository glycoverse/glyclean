# auto_remove includes QC samples in simple preset filtering

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group")
    Message
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

# auto_remove works with discovery preset

    Code
      res <- auto_remove(exp, preset = "discovery", group_col = "group")
    Message
      i Applying preset "discovery"...
      i Total removed: 2 (20%) variables.

# auto_remove works with biomarker preset

    Code
      res <- auto_remove(exp, preset = "biomarker", group_col = "group")
    Message
      i Applying preset "biomarker"...
      i Total removed: 2 (20%) variables.

# auto_remove handles no QC samples

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group")
    Message
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

