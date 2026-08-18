#' Automatical CoDA transformation
#'
#' If the data has more than 50 variables, call [transform_alr()].
#' Otherwise, call [transform_clr()],
#' following the same strategy in glycowork.
#'
#' @inheritParams transform_clr
#' @inheritSection transform_clr Algorithmic details
#' @inheritSection transform_clr Motif quantification
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#'
#' @returns The input container with a CoDA-transformed expression matrix (ALR
#'   if >50 variables, CLR otherwise) and its class preserved.
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
