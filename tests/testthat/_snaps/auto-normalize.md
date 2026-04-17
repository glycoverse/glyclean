# auto_normalize works with QC samples

    Code
      normed <- auto_normalize(exp, group_col = "group", qc_name = "QC")
    Message
      i QC samples found. Choosing the best normalization method based on QC samples.
      * Raw data: Median CV = CV_VALUE
      * Method "normalize_median": Median CV = CV_VALUE
      * Method "normalize_median_abs": Median CV = CV_VALUE
      * Method "normalize_total_area": Median CV = CV_VALUE
      * Method "normalize_quantile": Median CV = CV_VALUE
      * Method "normalize_median_quotient": Median CV = CV_VALUE
      * Method "normalize_loessf": Median CV = CV_VALUE
      * Method "normalize_loesscyc": Median CV = CV_VALUE
      * Method "normalize_rlr": Median CV = CV_VALUE
      * Method "normalize_rlrma": Median CV = CV_VALUE
      * Method "normalize_rlrmacyc": Median CV = CV_VALUE
      v Best method: "BEST_METHOD" with Median CV = CV_VALUE

# auto_normalize handles NULL qc_name

    Code
      normed <- auto_normalize(exp, group_col = "group", qc_name = NULL)
    Message
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "others". Using `normalize_median()`.

# auto_normalize works for glycoproteomics without QC

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "glycoproteomics". Using `normalize_median()`.

# auto_normalize falls back to median for others

    Code
      auto <- auto_normalize(exp, group_col = NULL)
    Message
      i No QC samples found. Using default normalization method based on experiment type.
      i Experiment type is "others". Using `normalize_median()`.

