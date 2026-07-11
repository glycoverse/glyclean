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
#' @param exp A [glyexp::experiment()] object. [glyexp::GlycomicSE()] and
#'   [glyexp::GlycoproteomicSE()] objects are also supported.
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#'
#' @returns The normalized [glyexp::experiment()] object. Glyco SE inputs return
#'   the same subclass.
#' @examples
#' library(glyexp)
#' exp_normed <- auto_normalize(real_experiment)
#' @export
auto_normalize <- function(
  exp,
  group_col = "group"
) {
  # Check arguments
  .assert_auto_container(exp)
  checkmate::assert_string(group_col, null.ok = TRUE)

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
