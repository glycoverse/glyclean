#' Automatical CoDA transformation
#'
#' If the data has more than 50 variables, call [transform_alr()].
#' Otherwise, call [transform_clr()],
#' following the same strategy in glycowork.
#' For glycoproteomics, this decision is made independently at each protein
#' site, using the number of features at that site.
#'
#' @inheritParams transform_clr
#' @inheritSection transform_clr Algorithmic details
#' @inheritSection transform_clr Glycoproteomics
#' @inheritSection transform_clr Motif quantification
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#'
#' @returns The input container with a CoDA-transformed expression matrix (ALR
#'   if >50 variables per composition, CLR otherwise) and its class preserved.
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
      "Applying CoDA separately at each protein site (ALR for >50 features, CLR otherwise)."
    )
    return(withr::with_seed(
      seed,
      .transform_coda_sites(
        x,
        method = "auto",
        by = by,
        gamma = gamma,
        group_scales = group_scales
      )
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
