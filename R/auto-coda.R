#' Automatical CoDA transformation
#'
#' If the data has more than 50 variables, call [transform_alr()].
#' Otherwise, call [transform_clr()],
#' following the same strategy in glycowork.
#'
#' @inheritParams transform_clr
#' @inheritSection transform_clr Algorithmic details
#' @inheritSection transform_clr Motif quantification
#'
#' @returns Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'  returns a `glyexp_experiment` with a CoDA-transformed expression matrix
#'  (ALR if >50 variables, CLR otherwise).
#' @export
auto_coda <- function(x, by = NULL, gamma = 0.1, group_scales = NULL) {
  if (nrow(x) > 50) {
    cli::cli_alert_info("Data has more than 50 variables, using ALR transformation.")
    transform_alr(x, by = by, gamma = gamma, group_scales = group_scales)
  } else {
    cli::cli_alert_info("Data has 50 or fewer variables, using CLR transformation.")
    transform_clr(x, by = by, gamma = gamma, group_scales = group_scales)
  }
}