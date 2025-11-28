#' Automatic Batch Correction
#'
#' Detects and corrects batch effects in the experiment.
#' If batch information is available,
#' this function performs ANOVA to detect batch effects.
#' If more than 30% (controlled by `prop_threshold`) of variables show significant batch effects (p < 0.05),
#' batch correction is performed using ComBat.
#' If group information exists,
#' it will be used as a covariate in both detection and correction
#' to preserve biological variation.
#' If no batch information is available,
#' the function will return the original experiment.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param batch_col The column name in sample_info for batches. Default is "batch".
#'   Can be NULL when no batch information is available.
#' @param prop_threshold The proportion of variables that must show significant batch effects to perform batch correction.
#'   Default is 0.3 (30%).
#' @param check_confounding Whether to check for confounding between batch and group variables.
#'   Default to TRUE.
#' @param confounding_threshold The threshold for Cramer's V to consider batch and group variables highly confounded.
#'   Only used when `check_confounding` is TRUE. Default to 0.4.
#' @param info Internal parameter used by [auto_clean()].
#'
#' @return A [glyexp::experiment()] object with batch effects corrected.
#'
#' @examples
#' exp <- glyexp::real_experiment
#' exp <- auto_correct_batch_effect(exp)
#'
#' @export
auto_correct_batch_effect <- function(
  exp,
  group_col = "group",
  batch_col = "batch",
  prop_threshold = 0.3,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  info = NULL
) {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_number(prop_threshold, lower = 0, upper = 1)

  # Check if batch column exists
  if (is.null(batch_col) || !batch_col %in% colnames(exp$sample_info)) {
    cli::cli_alert_info("Batch column {.field {batch_col}} not found in sample_info. Skipping batch correction.")
    return(exp)
  }

  # Check if group column exists
  actual_group_col <- NULL
  if (!is.null(group_col) && group_col %in% colnames(exp$sample_info)) {
    actual_group_col <- group_col
  }

  # Detect batch effects
  # We suppress messages from detect_batch_effect as we will report the summary ourselves
  p_values <- suppressMessages(detect_batch_effect(exp, batch = batch_col, group = actual_group_col))

  # Calculate proportion of significant variables (p < 0.05)
  significant_vars <- sum(p_values < 0.05, na.rm = TRUE)
  total_vars <- sum(!is.na(p_values))

  if (total_vars == 0) {
    cli::cli_alert_warning("No valid p-values from batch effect detection. Skipping correction.")
    return(exp)
  }

  prop_significant <- significant_vars / total_vars

  # Format percentage for display
  prop_pct <- sprintf("%.1f%%", prop_significant * 100)
  threshold_pct <- sprintf("%.1f%%", prop_threshold * 100)

  # Decide whether to correct
  if (prop_significant > prop_threshold) {
    cli::cli_alert_info("Batch effects detected in {.val {prop_pct}} of variables (threshold: {.val {threshold_pct}}). Performing batch correction.")

    # Perform correction
    exp <- correct_batch_effect(
      exp,
      batch = batch_col,
      group = actual_group_col,
      check_confounding = check_confounding,
      confounding_threshold = confounding_threshold
    )

    cli::cli_alert_success("Batch correction completed.")
  } else {
    cli::cli_alert_info("Batch effects detected in {.val {prop_pct}} of variables (<= {.val {threshold_pct}}). Skipping batch correction.")
  }

  exp
}