#' Automatic Data Preprocessing
#'
#' Perform automatic data preprocessing on glycoproteomics data.
#' Calling r`auto_clean(exp)` equals to:
#'
#' ```r
#' exp |>
#'   remove_missing_variables(prop = 0.5) |>
#'   normalize_median() |>
#'   impute_zero() |>
#'   aggregate(to_level = "gf")
#' ```
#'
#' @param exp A `glyexp_experiment` object containing glycoproteomics data.
#' @param to_level The aggregation level. Default is "gf" (glycoform).
#'   See [aggregate()] for details.
#'
#' @importFrom magrittr %>%
#'
#' @return A modified `glyexp_experiment` object
#' @export
auto_clean <- function(exp, to_level = c("gf", "gp", "gfs", "gps")) {
  checkmate::assert_class(exp, "glyexp_experiment")
  to_level <- rlang::arg_match(to_level)
  remove_missing_variables(exp, prop = 0.5) %>%
    normalize_median() %>%
    impute_zero() %>%
    aggregate(to_level = to_level)
}
