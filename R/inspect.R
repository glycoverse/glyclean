#' Inspect Experiments for Automatic Cleaning
#'
#' @param exp An [glyexp::experiment()].
#' @param group_col The column name in sample_info for groups. Default is "group".
#' @param qc_name The name of QC samples in the `group_col` column. Default is "QC".
#'   Only used when `group_col` is not NULL.
#'
#' @returns A list of metadata for automatic cleaning.
#' @noRd
inspect_experiment <- function(exp, group_col = "group", qc_name = "QC") {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col, null.ok = TRUE)
  checkmate::assert_string(qc_name)

  res <- list()

  # has_group: whether the experiment has group information
  if (!is.null(group_col) && group_col %in% colnames(exp$sample_info)) {
    res$has_group <- TRUE
  } else {
    res$has_group <- FALSE
  }

  # has_qc: whether the experiment has QC samples
  if (res$has_group && qc_name %in% exp$sample_info[[group_col]]) {
    res$has_qc <- TRUE
  } else {
    res$has_qc <- FALSE
  }

  # qc_samples: sample names of QC samples, NULL if no QC samples
  if (res$has_qc) {
    res$qc_samples <- exp$sample_info[["sample"]][exp$sample_info[[group_col]] == qc_name]
  } else {
    res$qc_samples <- NULL
  }

  res
}