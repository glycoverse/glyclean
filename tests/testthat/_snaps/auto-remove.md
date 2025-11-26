# auto_remove works with simple preset

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC")
    Message
      QC samples found. Excluding 2 QC samples from removal process.
      Applying preset "simple"...
      Total removed: 1 (10%) variables.

# auto_remove works with discovery preset

    Code
      res <- auto_remove(exp, preset = "discovery", group_col = "group", qc_name = "QC")
    Message
      QC samples found. Excluding 2 QC samples from removal process.
      Applying preset "discovery"...
      Total removed: 2 (20%) variables.

# auto_remove works with biomarker preset

    Code
      res <- auto_remove(exp, preset = "biomarker", group_col = "group", qc_name = "QC")
    Message
      QC samples found. Excluding 2 QC samples from removal process.
      Applying preset "biomarker"...
      Total removed: 2 (20%) variables.

# auto_remove handles no QC samples

    Code
      res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC")
    Message
      No QC samples found. Using all samples.
      Applying preset "simple"...
      Total removed: 1 (10%) variables.

