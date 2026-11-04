#' Transform Abundances to a Logarithmic Scale
#'
#' Explicitly convert the abundance assay to natural, base-2, or base-10
#' logarithms and update its scale metadata.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object. Legacy
#'   `glyexp_experiment` objects are also supported.
#' @param base Logarithm base: `2` (default), `exp(1)` for natural logarithms,
#'   or `10`. These correspond to `"log2"`, `"log"`, and `"log10"` metadata.
#'
#' @details
#' Generally, you do not need to call `transform_log()` in Glycoverse workflows:
#' packages including glystats and glyvis perform the log transformations needed
#' by their analyses or plots internally. Follow the input contract of the
#' downstream function. In particular, scale-aware glystats functions use scale
#' metadata, whereas glyvis plots that internally compute `log2(x + 1)` expect
#' raw abundances. This is not a guarantee that every downstream function accepts
#' pre-logged data. Use this function when explicitly stored log abundances are
#' required, for example for export or an external analysis.
#'
#' Input scale is read from metadata; absent metadata means `"raw"`.
#' Ordinary log inputs are converted directly between bases, without
#' exponentiation or taking a second logarithm. Matching input and output scales
#' leave the assay unchanged. `"logx"` is rejected because its original
#' abundances cannot be recovered from scale metadata alone.
#'
#' Raw abundances must be non-negative. Zeros become `-Inf`, and `NA` is
#' preserved. Ordinary log inputs may contain finite negative or zero
#' coordinates and `-Inf` (zero abundance). `NaN` and positive infinity are
#' rejected on every input scale. No pseudocount or imputation is applied.
#' If zeros represent missing measurements, use [zero_to_na()] and an
#' appropriate imputation method. Some downstream methods require finite values.
#' Logging a sparse raw assay may increase memory use because implicit zeros
#' become `-Inf`.
#'
#' @return A container of the same class as `x`, with its abundance assay and
#'   scale metadata updated. Annotations and other metadata are preserved, and
#'   a replayable step is appended to the preprocessing pipeline.
#' @examples
#' x <- SummarizedExperiment::SummarizedExperiment(
#'   assays = list(abundance = matrix(c(1, 2, 4, 8), nrow = 2))
#' )
#' logged <- transform_log(x)
#' SummarizedExperiment::assay(logged)
#' get_pipeline(logged)
#' @seealso [data_scales], [glyclean_pipeline()]
#' @export
transform_log <- function(x, base = 2) {
  record <- .pipeline_recorder(x)
  checkmate::assert_number(base, finite = TRUE)
  checkmate::assert_choice(base, c(2, exp(1), 10))
  .reject_logx(x)
  scale <- .get_scale(x)
  target <- c("log2", "log", "log10")[[match(base, c(2, exp(1), 10))]]
  mat <- .get_expr_mat(x)
  if (any(is.nan(mat)) || any(mat == Inf, na.rm = TRUE)) {
    cli::cli_abort(
      "Log transformation does not accept NaN or positive infinity."
    )
  }
  if (scale == "raw" && any(mat < 0, na.rm = TRUE)) {
    cli::cli_abort("Log transformation requires non-negative raw abundances.")
  }
  if (scale != target) {
    mat <- if (scale == "raw") {
      log(mat, base = base)
    } else {
      converted <- mat * (.log_base(scale) / .log_base(target))
      if (any(is.finite(mat) & !is.finite(converted))) {
        cli::cli_abort("Log-base conversion exceeds the numeric range.")
      }
      converted
    }
  }
  if (inherits(x, "glyexp_experiment")) {
    x$expr_mat <- mat
    x$meta_data$scale <- target
  } else {
    SummarizedExperiment::assay(x) <- mat
    S4Vectors::metadata(x)$scale <- target
  }
  record(x)
}
