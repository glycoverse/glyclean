# auto_remove works with simple preset

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC")
    Message
      i QC samples found. Excluding 2 QC samples from removal process.
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

# auto_remove handles NULL qc_name

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = NULL)
    Message
      i No QC samples found. Using all samples.
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

# auto_remove works with discovery preset

    Code
      res <- auto_remove(exp, preset = "discovery", group_col = "group", qc_name = "QC")
    Message
      i QC samples found. Excluding 2 QC samples from removal process.
      i Applying preset "discovery"...
      i Total removed: 2 (20%) variables.

# auto_remove works with biomarker preset

    Code
      res <- auto_remove(exp, preset = "biomarker", group_col = "group", qc_name = "QC")
    Message
      i QC samples found. Excluding 2 QC samples from removal process.
      i Applying preset "biomarker"...
      i Total removed: 2 (20%) variables.

# auto_remove handles no QC samples

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC")
    Message
      i No QC samples found. Using all samples.
      i Applying preset "simple"...
      i Total removed: 1 (10%) variables.

