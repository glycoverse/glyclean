#' Replace Zeros with Missing Values
#'
#' Replace exact zeros (including negative zero) in the abundance assay with
#' `NA`. Nonzero values and existing missing values are unchanged. This is an
#' explicit preprocessing step: use it only when zeros represent missing
#' measurements, before choosing an appropriate imputation method.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object. Legacy
#'   `glyexp_experiment` objects are also supported.
#'
#' @details Only the abundance assay is changed. Annotations and metadata are
#'   preserved, with this operation appended to the preprocessing pipeline.
#'   Integer assays retain integer storage. Replacing implicit zeros in a sparse
#'   assay requires storing those entries as missing values and can increase
#'   memory use substantially.
#'
#' @return A container of the same class as `x`, with zeros replaced by `NA`.
#' @seealso [impute_zero()], [glyclean_pipeline()], [get_pipeline()]
#' @export
zero_to_na <- function(x) {
  record <- .pipeline_recorder(x)
  record({
    mat <- .get_expr_mat(x)
    mat[!is.na(mat) & mat == 0] <- NA
    if (inherits(x, "glyexp_experiment")) {
      x$expr_mat <- mat
    } else {
      SummarizedExperiment::assay(x) <- mat
    }
    x
  })
}
