# auto_remove includes QC samples in simple preset filtering

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC")
    Message
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

# auto_remove handles NULL qc_name

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = NULL)
    Condition
      Warning:
      The `qc_name` argument of `auto_remove()` is deprecated as of glyclean 0.14.0.
      i This function no longer uses QC sample information and the `qc_name` parameter will be removed in a future release.
    Message
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

# auto_remove works with discovery preset

    Code
      res <- auto_remove(exp, preset = "discovery", group_col = "group", qc_name = "QC")
    Message
      i Applying preset "discovery"...
      i Total removed: 2 (20%) variables.

# auto_remove works with biomarker preset

    Code
      res <- auto_remove(exp, preset = "biomarker", group_col = "group", qc_name = "QC")
    Message
      i Applying preset "biomarker"...
      i Total removed: 2 (20%) variables.

# auto_remove handles no QC samples

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC")
    Message
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

