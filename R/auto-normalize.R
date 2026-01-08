#' Automatic Normalization
#'
#' This function automatically selects and applies the most suitable normalization method for the given dataset.
#' If Quality Control (QC) samples are present, the method that best stabilizes them
#' (i.e., yields the lowest median coefficient of variation) is chosen.
#' Otherwise, it defaults to median normalization for glycoproteomics data,
#' and a combination of median quotient and total area normalization for glycomics data.
#'
#' @details
#' By default, all normalization methods except for VSN are included for benchmarking.
#' VSN is excluded because it compresses fold change estimate significantly
#' thus not suitable for regular omics context.
#'
#' @param exp An [glyexp::experiment()].
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param qc_name The name of QC samples in the `group_col` column. Default is "QC".
#'   Only used when `group_col` is not NULL. Can be NULL when no QC samples are available.
#' @param to_try Normalization functions to try. A list. Default includes:
#'   - [normalize_median()]: median normalization
#'   - [normalize_median_abs()]: absolute median normalization
#'   - [normalize_total_area()]: total area mormalization
#'   - [normalize_quantile()]: quantile normalization
#'   - [normalize_loessf()]: LoessF normalization
#'   - [normalize_loesscyc()]: LoessCyc normalization
#'   - [normalize_median_quotient()]: median quitient normalization
#'   - [normalize_rlr()]: Robust Linear Regression normalization
#'   - [normalize_rlrma()]: Robust Linear Regression with Median Adjustment normalization
#'   - [normalize_rlrmacyc()]: Robust Linear Regression with Median Adjustment and Cyclic normalization
#' @param info Internal parameter used by [auto_clean()].
#'
#' @returns The normalized experiment.
#' @examples
#' library(glyexp)
#' exp_normed <- auto_normalize(real_experiment)
#' @export
auto_normalize <- function(exp, group_col = "group", qc_name = "QC", to_try = NULL, info = NULL) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col, null.ok = TRUE)
  checkmate::assert_string(qc_name, null.ok = TRUE)
  checkmate::assert_list(to_try, types = "function", null.ok = TRUE)

  # Default normalization methods to try
  if (is.null(to_try)) {
    to_try <- list(
      normalize_median = normalize_median,
      normalize_median_abs = normalize_median_abs,
      normalize_total_area = normalize_total_area,
      normalize_quantile = normalize_quantile,
      normalize_median_quotient = normalize_median_quotient,
      normalize_loessf = normalize_loessf,
      normalize_loesscyc = normalize_loesscyc,
      normalize_rlr = normalize_rlr,
      normalize_rlrma = normalize_rlrma,
      normalize_rlrmacyc = normalize_rlrmacyc
    )
  }

  # Get experiment inspection
  if (is.null(info)) {
    info <- inspect_experiment(exp, group_col = group_col, qc_name = qc_name)
  }

  if (info$has_qc) {
    .auto_normalize_with_qc(exp, to_try, info)
  } else {
    .auto_normalize_default(exp)
  }
}

.auto_normalize_with_qc <- function(exp, to_try, info) {
  cli::cli_alert_info("QC samples found. Choosing the best normalization method based on QC samples.")

  best_method <- NULL
  best_cv <- Inf
  best_exp <- NULL

  # Calculate CV for raw data
  raw_cv <- .calc_median_cv(exp$expr_mat[, info$qc_samples, drop = FALSE])
  cli::cli_ul("Raw data: Median CV = {.val {signif(raw_cv, 4)}}")

  for (method_name in names(to_try)) {
    method <- to_try[[method_name]]

    # Try normalization
    tryCatch({
      normed_exp <- method(exp)
      normed_mat <- normed_exp$expr_mat[, info$qc_samples, drop = FALSE]
      cv <- .calc_median_cv(normed_mat)

      cli::cli_ul("Method {.val {method_name}}: Median CV = {.val {signif(cv, 4)}}")

      if (cv < best_cv) {
        best_cv <- cv
        best_method <- method_name
        best_exp <- normed_exp
      }
    }, error = function(e) {
      cli::cli_alert_warning("Method {.val {method_name}} failed: {e$message}")
    })
  }

  if (!is.null(best_exp)) {
    cli::cli_alert_success("Best method: {.val {best_method}} with Median CV = {.val {signif(best_cv, 4)}}")
    best_exp
  } else {
    cli::cli_alert_warning("All normalization methods failed. Returning original experiment.")
    exp
  }
}

.auto_normalize_default <- function(exp) {
  cli::cli_alert_info("No QC samples found. Using default normalization method based on experiment type.")

  exp_type <- exp$meta_data$exp_type

  if (is.null(exp_type)) {
    # Fallback if exp_type is missing (though it should be there)
    cli::cli_alert_warning("Experiment type not found. Defaulting to median normalization.")
    return(normalize_median(exp))
  }

  if (exp_type == "glycomics") {
    cli::cli_alert_info("Experiment type is {.val glycomics}. Using {.fn normalize_median_quotient} + {.fn normalize_total_area}.")
    exp <- normalize_median_quotient(exp)
    exp <- normalize_total_area(exp)
    exp
  } else {
    # Default for glycoproteomics and others
    cli::cli_alert_info("Experiment type is {.val {exp_type}}. Using {.fn normalize_median}.")
    normalize_median(exp)
  }
}

.calc_median_cv <- function(mat) {
  # Calculate CV for each feature: sd / mean
  # Handle zeros and NAs
  row_means <- rowMeans(mat, na.rm = TRUE)
  row_sds <- apply(mat, 1, stats::sd, na.rm = TRUE)

  cvs <- row_sds / row_means

  # Remove infinite or NaN CVs (e.g. mean = 0)
  cvs <- cvs[is.finite(cvs)]

  if (length(cvs) == 0) return(NA)

  stats::median(cvs, na.rm = TRUE)
}