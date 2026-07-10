#' Automatic Normalization
#'
#' This function automatically applies a deterministic default normalization
#' method based on experiment type. Quality Control (QC) samples are not used
#' to benchmark, select, or report normalization behavior.
#'
#' @details
#' The automatic strategy uses these defaults:
#' - `glycomics`: [normalize_total_area()].
#' - `glycoproteomics`: [normalize_median()].
#' - missing or other experiment types: [normalize_median()].
#'
#' @param exp An [glyexp::experiment()].
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param qc_name `r lifecycle::badge("deprecated")` This function no longer
#'   uses this argument.
#'   This parameter is ignored and will be removed in a future release.
#' @param to_try `r lifecycle::badge("deprecated")`
#'   This parameter is no longer used and will be removed in a future release.
#'   The automatic strategy is now deterministic and does not require
#'   user-specified methods to try.
#'
#' @returns The normalized experiment.
#' @examples
#' library(glyexp)
#' exp_normed <- auto_normalize(real_experiment)
#' @export
auto_normalize <- function(
  exp,
  group_col = "group",
  qc_name = "QC",
  to_try = NULL
) {
  # Check arguments
  .assert_glyclean_container(exp)
  checkmate::assert_string(group_col, null.ok = TRUE)

  if (!identical(qc_name, "QC")) {
    lifecycle::deprecate_warn(
      when = "0.14.0",
      what = "auto_normalize(qc_name)",
      details = "This function no longer uses the `qc_name` parameter and it will be removed in a future release."
    )
  }
  if (!is.null(to_try)) {
    lifecycle::deprecate_warn(
      when = "0.14.0",
      what = "auto_normalize(to_try)",
      details = "The automatic normalization strategy is now deterministic and does not require user-specified methods to try. The `to_try` parameter will be removed in a future release."
    )
  }

  .auto_normalize_default(exp)
}

#' Apply the deterministic automatic normalization strategy
#'
#' @param exp A supported glyclean container.
#'
#' @returns The normalized experiment.
#' @noRd
.auto_normalize_default <- function(exp) {
  strategy <- .choose_auto_normalize_strategy(.get_exp_type(exp))

  cli::cli_alert_info(
    "Normalization method: {.fn {strategy$method_name}}"
  )
  cli::cli_alert_info(
    "Reason: default for {.val {strategy$exp_type}}."
  )

  switch(
    strategy$method_name,
    normalize_total_area = normalize_total_area(exp),
    normalize_median = normalize_median(exp)
  )
}

#' Choose the deterministic automatic normalization strategy
#'
#' @param exp_type Experiment type from [glyexp::get_exp_type()].
#'
#' @returns A list containing the reported experiment type and method name.
#' @noRd
.choose_auto_normalize_strategy <- function(exp_type) {
  checkmate::assert_string(exp_type, null.ok = TRUE)

  if (identical(exp_type, "glycomics")) {
    return(list(
      exp_type = "glycomics",
      method_name = "normalize_total_area"
    ))
  }

  if (is.null(exp_type)) {
    exp_type <- "missing experiment type"
  }

  list(
    exp_type = exp_type,
    method_name = "normalize_median"
  )
}
