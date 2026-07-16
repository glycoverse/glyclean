#' Automatic Removing Variables
#'
#' @description
#' This function uses preset rules to remove variables with low quality.
#' Available presets:
#' - "simple": remove variables with more than 50% missing values.
#' - "discovery": more lenient, remove variables with more than 80% missing values,
#'   but ensure less than 50% of missing values in at least one group.
#' - "biomarker": more strict, remove variables with more than 40% missing values,
#'   and ensure less than 60% of missing values in all groups.
#'
#' @param exp A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param preset One of "simple", "discovery", or "biomarker".
#'   Default "discovery" if group information is available, otherwise "simple".
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#'
#' @returns The filtered input container, with its class preserved.
#'
#' @examples
#' library(glyexp)
#' exp <- real_experiment
#' auto_remove(exp)
#'
#' @export
auto_remove <- function(
  exp,
  preset = "discovery",
  group_col = "group"
) {
  .assert_glyclean_container(exp)
  checkmate::assert_choice(preset, c("simple", "discovery", "biomarker"))
  checkmate::assert_string(group_col, null.ok = TRUE)

  sample_info <- .get_sample_info(exp)
  has_group <- !is.null(group_col) && group_col %in% colnames(sample_info)

  exp_filtered <- exp
  if (has_group && is.factor(sample_info[[group_col]])) {
    sample_info[[group_col]] <- droplevels(sample_info[[group_col]])
    exp_filtered <- .rebuild_container(exp_filtered, sample_info = sample_info)
  }

  use_group_col <- if (has_group) group_col else NULL

  cli::cli_alert_info("Applying preset {.val {preset}}...")

  if (preset == "simple") {
    exp_filtered <- suppressMessages(remove_rare(
      exp_filtered,
      prop = 0.5,
      min_n = 1
    ))
  } else if (preset == "discovery") {
    exp_filtered <- suppressMessages(remove_rare(
      exp_filtered,
      prop = 0.8,
      min_n = 1
    ))
    exp_filtered <- suppressMessages(remove_rare(
      exp_filtered,
      prop = 0.5,
      by = use_group_col,
      strict = FALSE,
      min_n = 1
    ))
  } else if (preset == "biomarker") {
    exp_filtered <- suppressMessages(remove_rare(
      exp_filtered,
      prop = 0.4,
      min_n = 1
    ))
    exp_filtered <- suppressMessages(remove_rare(
      exp_filtered,
      prop = 0.6,
      by = use_group_col,
      strict = TRUE,
      min_n = 1
    ))
  }

  kept_vars <- rownames(.get_expr_mat(exp_filtered))
  n_before <- nrow(.get_expr_mat(exp))
  n_after <- length(kept_vars)
  n_removed <- n_before - n_after

  if (n_removed > 0) {
    exp <- .subset_container(exp, rows = kept_vars)

    prop_removed <- round(n_removed / n_before * 100, 2)
    cli::cli_alert_info(
      "Total removed: {.val {n_removed}} ({.val {prop_removed}}%) variables."
    )
  } else {
    cli::cli_alert_info("No variables removed.")
  }

  exp
}
