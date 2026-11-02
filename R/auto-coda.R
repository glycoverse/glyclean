#' Automatical CoDA transformation
#'
#' For glycoproteomics, always call [transform_clr()] to transform each protein
#' site independently, regardless of the number of features at the site.
#' For other supported containers, call [transform_alr()] when the data has more
#' than 50 variables and [transform_clr()] otherwise, following glycowork.
#'
#' @inheritParams transform_clr
#' @inheritSection transform_clr Algorithmic details
#' @inheritSection transform_clr Glycoproteomics
#' @inheritSection transform_clr Motif quantification
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#'
#' @returns The input container with a CoDA-transformed expression matrix and
#'   its class preserved. Glycoproteomics always uses site-wise CLR and retains
#'   all rows. Other supported containers use ALR if there are more than 50
#'   variables, or CLR otherwise.
#' @export
auto_coda <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL,
  seed = 123
) {
  .assert_glyclean_container(x)
  checkmate::assert_int(seed, lower = 0)

  if (identical(.get_exp_type(x), "glycoproteomics")) {
    cli::cli_alert_info(
      "Applying CLR separately at each protein site."
    )
    return(transform_clr(
      x,
      by = by,
      gamma = gamma,
      group_scales = group_scales,
      seed = seed
    ))
  }

  if (nrow(x) > 50) {
    cli::cli_alert_info(
      "Data has more than 50 variables, using ALR transformation."
    )
    transform_alr(
      x,
      by = by,
      gamma = gamma,
      group_scales = group_scales,
      seed = seed
    )
  } else {
    cli::cli_alert_info(
      "Data has 50 or fewer variables, using CLR transformation."
    )
    transform_clr(
      x,
      by = by,
      gamma = gamma,
      group_scales = group_scales,
      seed = seed
    )
  }
}
