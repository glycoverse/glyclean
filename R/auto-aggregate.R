#' Automatic Aggregation
#'
#' Aggregates glycoproteomics data to "gfs" (glycoforms with structures) level
#' if the glycan structure column exists,
#' otherwise to "gf" (glycoforms with compositions) level.
#'
#' @param exp A [glyexp::experiment()] object with "glycoproteomics" type.
#'
#' @returns A modified [glyexp::experiment()] object with aggregated expression matrix and
#'   updated variable information.
#'
#' @examples
#' library(glyexp)
#' exp <- real_experiment
#' auto_aggregate(exp)
#'
#' @export
auto_aggregate <- function(exp) {
  checkmate::assert_class(exp, "glyexp_experiment")
  if (glyexp::get_exp_type(exp) != "glycoproteomics") {
    cli::cli_abort(c(
      "The experiment type must be {.val glycoproteomics}.",
      "x" = "Got {.val {glyexp::get_exp_type(exp)}}."
    ))
  }
  if ("glycan_structure" %in% colnames(glyexp::get_var_info(exp))) {
    cli::cli_inform("Aggregating to {.val gfs} level")
    glyclean_aggregate.glyexp_experiment(exp, to_level = "gfs")
  } else {
    cli::cli_inform("Aggregating to {.val gf} level")
    glyclean_aggregate.glyexp_experiment(exp, to_level = "gf")
  }
}
