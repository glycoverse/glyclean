#' Remove Variables with Missing Values
#'
#' @param exp An experiment object.
#' @param prop The proportion of missing values to use as a threshold.
#' Variables with missing values above this threshold will be removed.
#' Defaults to 0.5.
#' @param n The number of missing values to use as a threshold.
#' An alternative to `prop`.
#' @param by A string specifying a sample information column to stratify by.
#' Missing value counts or proportions will be calculated within each group.
#' @param strict Works with `by`. If `FALSE`, remove a variable only if
#' it passes the missing threshold in all groups.
#' If `TRUE`, remove a variable if it passes the missing threshold in any group.
#' See examples for more details.
#'
#' @examples
#' exp <- glyexp::toy_experiment()
#' exp$expr_mat[1, 1] <- NA    # V1: 1/6 missing
#' exp$expr_mat[2, 1:3] <- NA  # V2: 3/6 missing
#' exp$expr_mat[3, 1:5] <- NA  # V3: 5/6 missing
#' exp$expr_mat[4, 1:6] <- NA  # V4: 6/6 missing
#' exp$expr_mat
#'
#' # Remove variables with more than 50% missing values.
#' remove_missing_variables(exp, prop = 0.5)$expr_mat
#'
#' # Remove variables with more than 2 missing values.
#' remove_missing_variables(exp, n = 2)$expr_mat
#'
#' # Remove variables if they have more than 1 missing value in all groups.
#' # In another word, keep variables as long as they have 1 or 0 missing value
#' # in any group.
#' remove_missing_variables(exp, by = "group", strict = FALSE)$expr_mat
#'
#' # Keep only variables with no missing values.
#' remove_missing_variables(exp, prop = 0)$expr_mat
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return An experiment object with the variables removed.
#' @export
remove_missing_variables <- function(exp, prop = NULL, n = NULL, by = NULL, strict = FALSE) {
  if (is.null(n) && is.null(prop)) {
    prop <- 0.5
  }
  if (!is.null(n) && !is.null(prop)) {
    rlang::abort("Only one of `prop` or `n` can be provided.")
  }
  if (!is.null(prop)) {
    checkmate::assert_number(prop, lower = 0, upper = 1)
  }
  if (!is.null(n)) {
    checkmate::assert_number(n, lower = 0)
  }
  if (is.null(by)) {
    if (is.null(n)) {
      vars_to_remove <- rowMeans(is.na(exp$expr_mat)) > prop
    } else {
      vars_to_remove <- rowSums(is.na(exp$expr_mat)) > n
    }
  } else {
    # Calculate missing values by group
    groups <- split(seq_len(ncol(exp$expr_mat)), exp$sample_info[[by]])
    group_results <- lapply(groups, function(idx) {
      if (is.null(n)) {
        rowMeans(is.na(exp$expr_mat[, idx, drop = FALSE])) > prop
      } else {
        rowSums(is.na(exp$expr_mat[, idx, drop = FALSE])) > n
      }
    })

    # Combine results based on strict parameter
    if (strict) {
      # Remove if exceeds threshold in any group
      vars_to_remove <- Reduce(`|`, group_results)
    } else {
      # Remove only if exceeds threshold in all groups
      vars_to_remove <- Reduce(`&`, group_results)
    }
  }
  exp$expr_mat <- exp$expr_mat[!vars_to_remove, ]
  exp$var_info <- exp$var_info %>%
    dplyr::filter(.data$variable %in% rownames(exp$expr_mat))
  exp
}
