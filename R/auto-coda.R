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
  .run_recorded_coda(
    x, method = "auto", by = by, gamma = gamma,
    group_scales = group_scales, seed = seed
  )
}
