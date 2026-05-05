#' Inspect Experiments for Automatic Cleaning
#'
#' @param exp An [glyexp::experiment()].
#' @param group_col The column name in sample_info for groups. Default is "group".
#'
#' @returns A list of metadata for automatic cleaning.
#' @noRd
inspect_experiment <- function(exp, group_col = "group") {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col, null.ok = TRUE)

  list(
    has_group = !is.null(group_col) && group_col %in% colnames(exp$sample_info)
  )
}
