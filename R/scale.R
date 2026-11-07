#' Abundance scales in preprocessing and quality control
#'
#' Glyclean reads `scale` from experiment metadata. An absent field defaults to
#' `"raw"`; explicitly invalid values are rejected. `"log"` is the natural
#' logarithm, while `"log2"` and `"log10"` use their named bases. `"logx"` denotes
#' log-like coordinates whose original abundances cannot be recovered from
#' scale metadata alone. Ordinary processing converts to the algorithm's working
#' domain and restores the input scale. Filters return subsets of the original
#' assay without a numeric round trip. These conventions apply to legacy
#' experiments, glyco subclasses, and
#' plain `SummarizedExperiment` objects wherever the operation supports them.
#' Processing results from older `GlycomicSE` and `GlycoproteomicSE` objects
#' record an absent `scale` field as `"raw"` to satisfy current S4 validity,
#' including filters, skipped operations, and empty pipelines. The input is not
#' modified. Legacy S3 and plain SE containers retain an absent `scale` field
#' when no scale change is required. Read-only QC and history access do not
#' modify metadata.
#'
#' @section Working domains:
#' * Raw abundance: total-area, median, absolute-median, median-quotient and
#'   quantile normalization; MissForest and half-sample-minimum imputation;
#'   aggregation (including custom `f`); low-expression, variance and CV filters;
#'   total-intensity and CV plots. Custom aggregation callbacks receive raw
#'   values after missing values are removed. A sum over an all-missing group is
#'   zero, represented as `-Inf` on ordinary log scales.
#' * Log2 abundance: loess and robust-regression normalization; KNN, BPCA, PPCA,
#'   SVD and MinProb imputation; rank abundance, intensity boxplots, RLE, PCA and
#'   replicate scatter plots. RLE subtracts the row median in this working domain.
#'
#' Batch detection and correction both use `log2(raw + 1e-6)`.
#' Correction reverses this offset and truncates negative corrected abundances
#' to zero (`-Inf` on ordinary log scales). Skipped or failed corrections keep
#' the original assay exactly.
#'
#' Missingness and constancy are evaluated in the current representation.
#' Sample-minimum imputation also uses the current representation; wholly
#' missing samples remain missing. Half-minimum imputation likewise leaves
#' wholly missing samples missing.
#'
#' @section Log-like coordinates:
#' `"logx"` denotes log-like coordinates whose original abundance cannot be
#' recovered from the scale alone. Supported algorithms use these coordinates
#' directly, including legitimate negative values and zeros. No logarithm,
#' exponentiation, pseudocount, or automatic CLR recentering is applied.
#'
#' `logx` supports quantile, loess and robust-regression normalization; KNN,
#' BPCA, PPCA, SVD, MissForest and sample-minimum imputation; missingness and
#' constant-variable filters; batch detection/correction; missingness,
#' intensity, RLE, PCA and replicate plots; and annotation. Suitability of each
#' model for particular CoDA or VSN coordinates still depends on the data.
#'
#' Operations requiring recoverable abundances reject `logx`: total-area,
#' median, absolute-median and median-quotient normalization; half-minimum and
#' MinProb imputation; low-CV, variance and expression filters; aggregation;
#' TIC, CV and rank-abundance plots; and repeated CoDA or VSN transformations.
#' [impute_zero()] accepts only `raw`. [zero_to_na()] replaces zero on `raw`
#' and `-Inf` on ordinary log scales, preserving finite coordinate zero and
#' other non-finite values; it rejects `logx`.
#'
#' @section Transformations and automatic processing:
#' [transform_log()] explicitly converts ordinary abundance scales to `log`,
#' `log2`, or `log10`, updating metadata without adding a pseudocount. It is
#' generally unnecessary for downstream Glycoverse analyses and plots that
#' already transform their inputs; see its documentation for input contracts.
#'
#' [transform_clr()], [transform_alr()], [auto_coda()] and [normalize_vsn()]
#' accept ordinary scales and deliberately produce `scale = "logx"`. CoDA
#' returns log2-ratio coordinates and VSN returns its log-like coordinates.
#' CoDA requires finite, positive abundances (finite ordinary log input), with
#' no missing values. Other log-domain algorithms require finite observed
#' coordinates; use [zero_to_na()] and suitable imputation or filtering first.
#'
#' Automatic functions keep their existing algorithm selection and inherit that
#' algorithm's scale policy. [auto_clean()] and [auto_normalize()] reject
#' `logx`; [auto_impute()] rejects it when its selected algorithm is MinProb.
#' Pipelines store the actual processing function and parameters and perform no
#' hidden scale conversion. ALR fallback records CLR, restarting from the
#' supplied seed so its history faithfully replays the fallback transformation.
#'
#' Protein adjustment has a separate abundance contract; see [adjust_protein()].
#' Its scale API is unchanged by this adaptation.
#'
#' @name data_scales
NULL

# Internal scale conversion shared by preprocessing and QC. `logx` is never
# exponentiated: it is a coordinate system, not a known logarithm of abundance.
.get_scale <- function(x) {
  metadata <- .get_container_metadata(x)
  # Only a missing field denotes a legacy raw object, not an explicit NULL.
  if (!"scale" %in% names(metadata)) {
    return("raw")
  }
  scale <- metadata[["scale"]]
  checkmate::assert_choice(scale, c("raw", "log", "log2", "log10", "logx"))
  scale
}


# Materialize the legacy default only where current glyco S4 validity requires
# it. Ordinary SE and legacy S3 containers can retain their original metadata.
# Read through .get_scale() first so an explicit invalid value is never repaired.
.ensure_scale_metadata <- function(x) {
  scale <- .get_scale(x)
  if (methods::is(x, "GlycomicSE") || methods::is(x, "GlycoproteomicSE")) {
    metadata <- .get_container_metadata(x)
    if (!"scale" %in% names(metadata)) {
      metadata$scale <- scale
      S4Vectors::metadata(x) <- metadata
    }
  }
  x
}

.reject_logx <- function(x) {
  if (.get_scale(x) == "logx") {
    cli::cli_abort(
      "This operation requires recoverable abundances and does not support scale = 'logx'."
    )
  }
  invisible(x)
}

.log_base <- function(scale) {
  switch(scale, log = 1, log2 = log(2), log10 = log(10))
}

# Convert known log scales directly, avoiding an unnecessary exponential round trip.
.to_work_scale <- function(mat, scale, work_scale = "raw", allow_logx = FALSE) {
  if (scale == "logx") {
    if (!allow_logx) {
      cli::cli_abort("This operation does not support scale = 'logx'.")
    }
    return(mat)
  }
  if (work_scale == "current" || scale == work_scale) {
    return(mat)
  }
  if (work_scale == "raw") {
    return(exp(mat * .log_base(scale)))
  }
  if (work_scale == "log2") {
    if (scale == "raw") {
      return(suppressWarnings(log2(mat)))
    }
    return(mat * (.log_base(scale) / log(2)))
  }
  cli::cli_abort("Unknown internal working scale.")
}

.from_work_scale <- function(mat, scale, work_scale = "raw") {
  if (scale == "logx" || work_scale == "current" || scale == work_scale) {
    return(mat)
  }
  if (work_scale == "raw") {
    return(suppressWarnings(log(mat) / .log_base(scale)))
  }
  if (scale == "raw") {
    return(2^mat)
  }
  mat * (log(2) / .log_base(scale))
}

.assert_finite_observed <- function(mat) {
  if (any(is.nan(mat)) || any(!is.na(mat) & !is.finite(mat))) {
    cli::cli_abort(c(
      "This operation requires finite observed values in its working domain.",
      "i" = "Use zero_to_na() for zero abundances and choose appropriate imputation or filtering for missing values. No pseudocount is added."
    ))
  }
  invisible(mat)
}

.work_matrix <- function(x, work_scale = "raw", allow_logx = FALSE) {
  mat <- .to_work_scale(.get_expr_mat(x), .get_scale(x), work_scale, allow_logx)
  if (work_scale != "current") {
    .assert_finite_observed(mat)
  }
  mat
}

.update_scaled_expr_mat <- function(
  exp,
  matrix_func,
  by = NULL,
  ...,
  work_scale = "raw",
  allow_logx = FALSE,
  output_scale = NULL
) {
  scale <- .get_scale(exp)
  mat <- .work_matrix(exp, work_scale, allow_logx)
  sample_info <- .get_sample_info(exp)
  by_values <- .resolve_column_param(by, sample_info, "by", ncol(mat))
  if (is.character(by) && length(by) == 1L && by == "sample") {
    cli::cli_abort("Can't stratify by {.val {by}}.")
  }
  result <- .apply_by_groups(mat, matrix_func, by_values, ...)
  if (is.null(output_scale)) {
    result <- .from_work_scale(result, scale, work_scale)
    metadata <- .get_container_metadata(exp)
  } else {
    metadata <- .get_container_metadata(exp)
    metadata$scale <- output_scale
  }
  .rebuild_container(exp, expr_mat = result, metadata = metadata)
}

# log2(raw + offset), evaluated without overflowing or underflowing raw.
.batch_work_matrix <- function(x, offset = 1e-6) {
  scale <- .get_scale(x)
  mat <- .get_expr_mat(x)
  if (scale == "logx") {
    .assert_finite_observed(mat)
    return(mat)
  }
  if (scale == "raw") {
    if (any(mat < 0, na.rm = TRUE)) {
      cli::cli_abort("Batch analysis requires non-negative abundances.")
    }
    result <- log2(mat + offset)
  } else {
    logs <- .to_work_scale(mat, scale, "log2")
    high <- pmax(logs, log2(offset))
    result <- high + log2(2^(logs - high) + 2^(log2(offset) - high))
  }
  .assert_finite_observed(result)
  result
}

.batch_from_work <- function(mat, scale, offset = 1e-6) {
  if (scale == "logx") {
    return(mat)
  }
  delta <- (mat - log2(offset)) * log(2)
  if (scale == "raw") {
    return(2^mat * (-expm1(-pmax(delta, 0))))
  }
  # Values below the offset represent negative corrected abundances and are
  # truncated to zero, as for ordinary raw data. log(0) is -Inf.
  logs <- mat * log(2) + log(-expm1(-pmax(delta, 0)))
  logs / .log_base(scale)
}

.qc_log_matrix <- function(x, allow_logx = TRUE, sample_names = NULL) {
  mat <- .to_work_scale(.get_expr_mat(x), .get_scale(x), "log2", allow_logx)
  mat[!is.finite(mat)] <- NA_real_
  if (!is.null(sample_names)) {
    colnames(mat) <- sample_names
  }
  mat
}

.qc_log_label <- function(x) {
  if (.get_scale(x) == "logx") "Log-like coordinate" else "Log2 intensity"
}

.log2_sum <- function(values) {
  values <- values[!is.na(values)]
  if (!length(values) || all(values == -Inf)) {
    return(-Inf)
  }
  # Zero references can produce infinite ratios inside median-quotient normalization.
  if (any(values == Inf)) {
    return(Inf)
  }
  high <- max(values)
  high + log2(sum(2^(values - high)))
}

# The logarithm of R's median, including its arithmetic mean for even n.
.log2_median <- function(values) {
  values <- sort(values[!is.na(values)])
  n <- length(values)
  if (!n) {
    return(NA_real_)
  }
  if (n %% 2L) {
    return(values[(n + 1L) %/% 2L])
  }
  .log2_sum(values[c(n %/% 2L, n %/% 2L + 1L)]) - 1
}

.normalize_known_log <- function(x, method, by = NULL) {
  if (method %in% c("median", "median_abs") && ncol(.get_expr_mat(x)) == 1L) {
    return(x)
  }
  mat <- .to_work_scale(.get_expr_mat(x), .get_scale(x), "log2")
  if (any(is.nan(mat)) || any(mat == Inf, na.rm = TRUE)) {
    cli::cli_abort(
      "Normalization requires finite log abundances or -Inf for zero abundance."
    )
  }
  by_values <- .resolve_column_param(by, .get_sample_info(x), "by", ncol(mat))
  if (is.character(by) && length(by) == 1L && by == "sample") {
    cli::cli_abort("Can't stratify by {.val {by}}.")
  }
  result <- .apply_by_groups(
    mat,
    function(values) {
      if (method == "total_area") {
        centers <- vapply(
          seq_len(ncol(values)),
          function(j) .log2_sum(values[, j]),
          numeric(1)
        )
      } else if (method %in% c("median", "median_abs")) {
        if (ncol(values) == 1L) {
          return(values)
        }
        centers <- vapply(
          seq_len(ncol(values)),
          function(j) .log2_median(values[, j]),
          numeric(1)
        )
        centers <- centers - mean(centers)
      } else {
        ref <- apply(values, 1, .log2_median)
        quotients <- sweep(values, 1, ref, "-")
        centers <- vapply(
          seq_len(ncol(values)),
          function(j) .log2_median(quotients[, j]),
          numeric(1)
        )
      }
      sweep(values, 2, centers, "-")
    },
    by_values
  )
  .rebuild_container(
    x,
    expr_mat = .from_work_scale(result, .get_scale(x), "log2")
  )
}
