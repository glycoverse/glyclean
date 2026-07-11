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
#' @param exp A [glyexp::experiment()] or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#'
#' @returns The imputed input container. SummarizedExperiment inputs return the
#'   same class.
#' @examples
#' library(glyexp)
#' exp_imputed <- auto_impute(real_experiment)
#' @export
auto_impute <- function(
  exp,
  group_col = "group"
) {
  # Check arguments
  .assert_glyclean_container(exp)
  checkmate::assert_string(group_col, null.ok = TRUE)

  .auto_impute_default(exp)
}

#' Apply the deterministic automatic imputation strategy
#'
#' @param exp A supported glyclean container.
#'
#' @returns The imputed experiment.
#' @noRd
.auto_impute_default <- function(exp) {
  n_samples <- ncol(exp)
  exp_type <- .get_exp_type(exp)
  strategy <- .choose_auto_impute_strategy(n_samples, exp_type)

  cli::cli_alert_info(
    "Imputation method: {.fn {strategy$method_name}}"
  )
  cli::cli_alert_info(
    "Reason: default for {.val {strategy$exp_type}} with {strategy$sample_group}."
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

  if (identical(exp_type, "others")) {
    return(list(
      exp_type = exp_type,
      sample_group = stringr::str_glue("n_samples = {n_samples}"),
      method_name = "impute_min_prob"
    ))
  }
  if (exp_type %in% c("traitomics", "traitproteomics")) {
    cli::cli_abort(c(
      "Can only apply automatic imputation on glycomics and glycoproteomics experiments.",
      "x" = "Experiment type {.val {exp_type}} is not supported."
    ))
  }

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
