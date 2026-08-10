#' Automatic Aggregation
#'
#' Aggregates glycomics or glycoproteomics data to a structure-aware level when
#' the glycan structure column exists, and to a composition-only level otherwise.
#' Glycomics data is aggregated to "gs" or "g"; glycoproteomics data is
#' aggregated to "gfs" or "gf".
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
#' auto_aggregate(exp)
#'
#' @export
auto_aggregate <- function(exp, standardize_variable = TRUE) {
  .assert_auto_container(exp)
  exp_type <- .get_exp_type(exp)
  if (!exp_type %in% c("glycomics", "glycoproteomics")) {
    cli::cli_abort(c(
      "The experiment type must be {.val glycomics} or {.val glycoproteomics}.",
      "x" = "Got {.val {exp_type}}."
    ))
  }
  has_structure <- "glycan_structure" %in% colnames(.get_var_info(exp))
  to_level <- if (exp_type == "glycomics") {
    if (has_structure) "gs" else "g"
  } else {
    if (has_structure) "gfs" else "gf"
  }
  cli::cli_alert_info("Aggregating to {.val {to_level}} level")
  .aggregate_container(
    exp,
    to_level = to_level,
    standardize_variable = standardize_variable,
    error_call = rlang::caller_call()
  )
}
