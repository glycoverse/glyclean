#' Automatic Imputation
#'
#' This function automatically selects and applies the most suitable imputation method for the given dataset.
#' If Quality Control (QC) samples are present, the method that best stabilizes them
#' (i.e., yields the lowest median coefficient of variation) is chosen.
#' Otherwise, it defaults to a sample-size-based strategy:
#' - less than 30 samples: Sample minimum imputation
#' - between 30 and 100 samples: Minimum probability imputation
#' - more than 100 samples: MissForest imputation
#'
#' @details
#' By default, all imputation methods are included for benchmarking when QC samples are available.
#' Note that some methods (e.g., MissForest) may be slow for large datasets.
#'
#' @param exp An [glyexp::experiment()].
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param qc_name The name of QC samples in the `group_col` column. Default is "QC".
#'   Only used when `group_col` is not NULL.
#' @param to_try Imputation functions to try. A list. Default includes:
#'   - [impute_zero()]: zero imputation
#'   - [impute_sample_min()]: sample minimum imputation
#'   - [impute_half_sample_min()]: half sample minimum imputation
#'   - [impute_sw_knn()]: sample-wise KNN imputation
#'   - [impute_fw_knn()]: feature-wise KNN imputation
#'   - [impute_bpca()]: BPCA imputation
#'   - [impute_ppca()]: PPCA imputation
#'   - [impute_svd()]: SVD imputation
#'   - [impute_min_prob()]: minimum probability imputation
#'   - [impute_miss_forest()]: MissForest imputation
#' @param info Internal parameter used by [auto_clean()].
#'
#' @returns The imputed experiment.
#' @examples
#' library(glyexp)
#' exp_imputed <- auto_impute(real_experiment)
#' @export
auto_impute <- function(exp, group_col = "group", qc_name = "QC", to_try = NULL, info = NULL) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col, null.ok = TRUE)
  checkmate::assert_string(qc_name)
  checkmate::assert_list(to_try, types = "function", null.ok = TRUE)

  # Default imputation methods to try
  if (is.null(to_try)) {
    to_try <- list(
      impute_zero = impute_zero,
      impute_sample_min = impute_sample_min,
      impute_half_sample_min = impute_half_sample_min,
      impute_sw_knn = impute_sw_knn,
      impute_fw_knn = impute_fw_knn,
      impute_bpca = impute_bpca,
      impute_ppca = impute_ppca,
      impute_svd = impute_svd,
      impute_min_prob = impute_min_prob,
      impute_miss_forest = impute_miss_forest
    )
  }

  # Get experiment inspection
  if (is.null(info)) {
    info <- inspect_experiment(exp, group_col = group_col, qc_name = qc_name)
  }

  if (info$has_qc) {
    .auto_impute_with_qc(exp, to_try, info)
  } else {
    .auto_impute_default(exp)
  }
}

.auto_impute_with_qc <- function(exp, to_try, info) {
  cli::cli_inform("QC samples found. Choosing the best imputation method based on QC samples.")

  best_method <- NULL
  best_cv <- Inf
  best_exp <- NULL

  # Calculate CV for raw data (excluding NA values)
  raw_cv <- .calc_median_cv(exp$expr_mat[, info$qc_samples, drop = FALSE])
  cli::cli_ul("Raw data: Median CV = {.val {signif(raw_cv, 4)}}")

  for (method_name in names(to_try)) {
    method <- to_try[[method_name]]

    # Try imputation
    tryCatch({
      suppressWarnings(imputed_exp <- method(exp))
      imputed_mat <- imputed_exp$expr_mat[, info$qc_samples, drop = FALSE]
      cv <- .calc_median_cv(imputed_mat)

      cli::cli_ul("Method {.val {method_name}}: Median CV = {.val {signif(cv, 4)}}")

      if (cv < best_cv) {
        best_cv <- cv
        best_method <- method_name
        best_exp <- imputed_exp
      }
    }, error = function(e) {
      cli::cli_warn("Method {.val {method_name}} failed: {e$message}")
    })
  }

  if (!is.null(best_exp)) {
    cli::cli_alert_success("Best method: {.val {best_method}} with Median CV = {.val {signif(best_cv, 4)}}")
    best_exp
  } else {
    cli::cli_warn("All imputation methods failed. Returning original experiment.")
    exp
  }
}

.auto_impute_default <- function(exp) {
  cli::cli_inform("No QC samples found. Using default imputation method based on sample size.")

  n_samples <- ncol(exp)

  if (n_samples <= 30) {
    cli::cli_inform("Sample size <= 30, using {.fn impute_sample_min}.")
    impute_sample_min(exp)
  } else if (n_samples <= 100) {
    cli::cli_inform("Sample size <= 100, using {.fn impute_min_prob}.")
    impute_min_prob(exp)
  } else {
    cli::cli_inform("Sample size > 100, using {.fn impute_miss_forest}.")
    impute_miss_forest(exp)
  }
}
