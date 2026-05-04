#' Automatic Imputation
#'
#' This function automatically selects and applies a deterministic default
#' imputation method by sample count and experiment type.
#' Quality Control (QC) samples are inspected for workflow consistency, but they
#' are not used to benchmark or select the imputation method.
#'
#' @details
#' The automatic strategy uses these defaults:
#' - `n_samples < 30`: [impute_min_prob()] for glycomics and glycoproteomics.
#' - `30 <= n_samples <= 100`: [impute_bpca()] for glycomics and
#'   [impute_min_prob()] for glycoproteomics.
#' - `n_samples > 100`: [impute_miss_forest()] for glycomics and
#'   [impute_bpca()] for glycoproteomics.
#'
#' Other experiment types use the glycoproteomics defaults as a conservative
#' fallback. [impute_sample_min()] and [impute_half_sample_min()] remain
#' available for manual use, but they are not selected automatically.
#'
#' @param exp An [glyexp::experiment()].
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param qc_name `r lifecycle::badge("deprecated")` This function no longer uses QC sample information.
#'   This parameter is ignored and will be removed in a future release.
#' @param to_try `r lifecycle::badge("deprecated")`
#'   This parameter is no longer used and will be removed in a future release.
#'   The automatic strategy is now deterministic and does not require user-specified methods to try.
#' @param info Internal parameter used by [auto_clean()].
#'
#' @returns The imputed experiment.
#' @examples
#' library(glyexp)
#' exp_imputed <- auto_impute(real_experiment)
#' @export
auto_impute <- function(
  exp,
  group_col = "group",
  qc_name = "QC",
  to_try = NULL,
  info = NULL
) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col, null.ok = TRUE)
  checkmate::assert_string(qc_name, null.ok = TRUE)
  checkmate::assert_list(to_try, types = "function", null.ok = TRUE)

  # Get experiment inspection
  if (is.null(info)) {
    info <- inspect_experiment(exp, group_col = group_col, qc_name = qc_name)
  }

  .auto_impute_default(exp, info)
}

#' Apply the deterministic automatic imputation strategy
#'
#' @param exp An [glyexp::experiment()].
#' @param info Experiment inspection metadata from `inspect_experiment()`.
#'
#' @returns The imputed experiment.
#' @noRd
.auto_impute_default <- function(exp, info) {
  if (info$has_qc) {
    cli::cli_alert_info(
      "QC samples found. Using deterministic default imputation method."
    )
  } else {
    cli::cli_alert_info(
      "No QC samples found. Using deterministic default imputation method."
    )
  }

  n_samples <- ncol(exp)
  exp_type <- glyexp::get_exp_type(exp)
  strategy <- .choose_auto_impute_strategy(n_samples, exp_type)

  cli::cli_alert_info(
    "Using default imputation method for {.val {strategy$exp_type}} with {strategy$sample_group}: {.fn {strategy$method_name}}."
  )

  switch(
    strategy$method_name,
    impute_min_prob = impute_min_prob(exp),
    impute_bpca = impute_bpca(exp),
    impute_miss_forest = impute_miss_forest(exp)
  )
}

#' Choose the deterministic automatic imputation strategy
#'
#' @param n_samples Number of samples in the experiment.
#' @param exp_type Experiment type from [glyexp::get_exp_type()].
#'
#' @returns A list containing the normalized experiment type, sample-count
#'   group label, and method name.
#' @noRd
.choose_auto_impute_strategy <- function(n_samples, exp_type) {
  checkmate::assert_count(n_samples, positive = TRUE)
  checkmate::assert_string(exp_type, null.ok = TRUE)

  if (identical(exp_type, "glycomics")) {
    strategy_exp_type <- "glycomics"
  } else {
    strategy_exp_type <- "glycoproteomics"
  }

  if (n_samples < 30) {
    sample_group <- "n_samples < 30"
    method_name <- "impute_min_prob"
  } else if (n_samples <= 100) {
    sample_group <- "30 <= n_samples <= 100"
    method_name <- switch(
      strategy_exp_type,
      glycomics = "impute_bpca",
      glycoproteomics = "impute_min_prob"
    )
  } else {
    sample_group <- "n_samples > 100"
    method_name <- switch(
      strategy_exp_type,
      glycomics = "impute_miss_forest",
      glycoproteomics = "impute_bpca"
    )
  }

  list(
    exp_type = strategy_exp_type,
    sample_group = sample_group,
    method_name = method_name
  )
}
