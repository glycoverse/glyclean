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
#' QC samples will not be considered in the removal process.
#'
#' @param exp A glyexp_experiment object.
#' @param preset One of "simple", "discovery", or "biomarker".
#'   Default "discovery" if group information is available, otherwise "simple".
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param qc_name The name of QC samples in the `group_col` column. Default is "QC".
#'   Only used when `group_col` is not NULL.
#' @param info Internal parameter used by [auto_clean()].
auto_remove <- function(exp, preset = "discovery", group_col = "group", qc_name = "QC", info = NULL) {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_choice(preset, c("simple", "discovery", "biomarker"))
  checkmate::assert_string(group_col, null.ok = TRUE)
  checkmate::assert_string(qc_name)

  if (is.null(info)) {
    info <- inspect_experiment(exp, group_col = group_col, qc_name = qc_name)
  }

  # Identify samples to use (exclude QC)
  all_samples <- exp$sample_info$sample
  if (info$has_qc) {
    samples_to_use <- setdiff(all_samples, info$qc_samples)
    cli::cli_inform("QC samples found. Excluding {.val {length(info$qc_samples)}} QC samples from removal process.")
  } else {
    samples_to_use <- all_samples
    cli::cli_inform("No QC samples found. Using all samples.")
  }

  if (length(samples_to_use) == 0) {
    cli::cli_abort("No samples left after excluding QC samples.")
  }

  # Create a subset experiment for calculation
  exp_sub <- exp
  exp_sub$expr_mat <- exp$expr_mat[, samples_to_use, drop = FALSE]
  exp_sub$sample_info <- exp$sample_info[exp$sample_info$sample %in% samples_to_use, , drop = FALSE]

  # Drop unused levels in group column if present to avoid empty groups in removal functions
  if (info$has_group && is.factor(exp_sub$sample_info[[group_col]])) {
    exp_sub$sample_info[[group_col]] <- droplevels(exp_sub$sample_info[[group_col]])
  }

  # Determine group column to use
  use_group_col <- if (info$has_group) group_col else NULL

  cli::cli_inform("Applying preset {.val {preset}}...")

  if (preset == "simple") {
    exp_sub <- suppressMessages(remove_rare(exp_sub, prop = 0.5, min_n = 1))
  } else if (preset == "discovery") {
    exp_sub <- suppressMessages(remove_rare(exp_sub, prop = 0.8, min_n = 1))
    exp_sub <- suppressMessages(remove_rare(exp_sub, prop = 0.5, by = use_group_col, strict = FALSE, min_n = 1))
  } else if (preset == "biomarker") {
    exp_sub <- suppressMessages(remove_rare(exp_sub, prop = 0.4, min_n = 1))
    exp_sub <- suppressMessages(remove_rare(exp_sub, prop = 0.6, by = use_group_col, strict = TRUE, min_n = 1))
  }

  # Apply removal to original experiment
  kept_vars <- rownames(exp_sub$expr_mat)
  n_before <- nrow(exp$expr_mat)
  n_after <- length(kept_vars)
  n_removed <- n_before - n_after

  if (n_removed > 0) {
    exp$expr_mat <- exp$expr_mat[kept_vars, , drop = FALSE]
    exp$var_info <- exp$var_info |> dplyr::filter(.data$variable %in% kept_vars)

    prop_removed <- round(n_removed / n_before * 100, 2)
    cli::cli_inform("Total removed: {.val {n_removed}} ({.val {prop_removed}}%) variables.")
  } else {
    cli::cli_inform("No variables removed.")
  }

  exp
}