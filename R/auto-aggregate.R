#' Automatic Aggregation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `auto_aggregate()` was deprecated in glyclean 0.15.3. Use [aggregate()]
#' instead; it now selects the aggregation level from the input type and whether
#' `glycan_structure` is present.
#'
#' @param exp A glycomics or glycoproteomics container: a
#'   [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or legacy
#'   `glyexp_experiment` object.
#' @param standardize_variable Whether to call [glyexp::standardize_variable()]
#'   after aggregation. Set to `FALSE` to skip network calls for faster testing.
#'   Default is `TRUE`.
#'
#' @returns A modified container with the same class as `exp`, an aggregated
#'   expression matrix, and updated variable information.
#'
#' @examples
#' library(glyexp)
#' exp <- real_experiment
#' # Deprecated:
#' # auto_aggregate(exp)
#'
#' # Use instead:
#' aggregate(exp)
#'
#' @keywords internal
#' @export
auto_aggregate <- function(exp, standardize_variable = TRUE) {
  lifecycle::deprecate_warn("0.15.3", "auto_aggregate()", "aggregate()")
  aggregate(
    exp,
    standardize_variable = standardize_variable
  )
}
