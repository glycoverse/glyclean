# auto_normalize works with QC samples

    Code
      normed <- auto_normalize(exp, group_col = "group", qc_name = "QC")
    Message
      QC samples found. Choosing the best normalization method based on QC samples.
      Median CV of raw data: 0.07814
      * Method "normalize_median": Median CV = 0.002167
      * Method "normalize_median_abs": Median CV = 0.002167
      * Method "normalize_total_area": Median CV = 0.002167
      * Method "normalize_quantile": Median CV = 0
      * Method "normalize_median_quotient": Median CV = 0.002167
      * Method "normalize_loessf": Median CV = 4.539e-06
      * Method "normalize_loesscyc": Median CV = 6.451e-05
      * Method "normalize_rlr": Median CV = 5.19e-05
      * Method "normalize_rlrma": Median CV = 5.182e-06
      * Method "normalize_rlrmacyc": Median CV = 0.0002218
      v Best method: "normalize_quantile" with Median CV = 0

# auto_normalize works for glycomics without QC

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      No QC samples found. Using default normalization method based on experiment type.
      Experiment type is "glycomics". Using `normalize_median_quotient()` + `normalize_total_area()`.

# auto_normalize works for glycoproteomics without QC

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      No QC samples found. Using default normalization method based on experiment type.
      Experiment type is "glycoproteomics". Using `normalize_median()`.

# auto_normalize falls back to median for others

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      No QC samples found. Using default normalization method based on experiment type.
      Experiment type is "others". Using `normalize_median()`.

