#' Zero Imputation
#'
#' Impute missing values in an expression matrix by replacing them with zeros.
#' Only `glyexp` experiments with `scale = "raw"` are supported.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_zero <- function(x) {
  record <- .pipeline_recorder(x)
  record({
    if (.get_scale(x) != "raw") {
      cli::cli_abort("impute_zero() only supports scale = 'raw'.")
    }
    .update_expr_mat(x, .impute_zero)
  })
}


#' Sample Minimum Imputation
#'
#' Impute missing values with the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' See also [impute_half_sample_min()].
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_sample_min <- function(x) {
  record <- .pipeline_recorder(x)
  record({
    .update_scaled_expr_mat(
      x,
      .impute_sample_min,
      work_scale = "current",
      allow_logx = TRUE
    )
  })
}


#' Half Sample Minimum Imputation
#'
#' Impute missing values with half of the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' Compared to [impute_sample_min()], this method is more conservative.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_half_sample_min <- function(x) {
  record <- .pipeline_recorder(x)
  record({
    .reject_logx(x)
    if (.get_scale(x) == "raw") {
      .update_expr_mat(x, .impute_half_sample_min)
    } else {
      .update_scaled_expr_mat(
        x,
        .impute_half_sample_min_log,
        work_scale = "current",
        .half_log = log(2) / .log_base(.get_scale(x))
      )
    }
  })
}


#' Sample-wise KNN Imputation
#'
#' A wrapper around the [impute::impute.knn()].
#' Impute missing values with values from the k-nearest neighbors of the
#' corresponding sample.
#' If there are strong patterns among the samples (such as group clustering
#' relationships or experimental conditions), this method can better utilize
#' the overall relationships among samples.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param k The number of nearest neighbors to consider.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param seed Non-negative integer seed for random number generation. Default is
#'   `123`. The caller's random state is restored, including on errors.
#' @param ... Additional arguments to pass to `impute::impute.knn()`.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_sw_knn <- function(x, k = 5, by = NULL, ..., seed = 123) {
  record <- .pipeline_recorder(x)
  checkmate::assert_int(seed, lower = 0)
  record(withr::with_seed(
    seed,
    .update_scaled_expr_mat(
      x,
      .impute_sw_knn,
      work_scale = "log2",
      allow_logx = TRUE,
      .log_input = TRUE,
      by = by,
      k = k,
      rng.seed = seed,
      ...
    )
  ))
}


#' Feature-wise KNN Imputation
#'
#' A wrapper around the [impute::impute.knn()].
#' Impute missing values with values from the k-nearest neighbors of the
#' corresponding feature.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param k The number of nearest neighbors to consider.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param seed Non-negative integer seed for random number generation. Default is
#'   `123`. The caller's random state is restored, including on errors.
#' @param ... Additional arguments to pass to `impute::impute.knn()`.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_fw_knn <- function(x, k = 5, by = NULL, ..., seed = 123) {
  record <- .pipeline_recorder(x)
  checkmate::assert_int(seed, lower = 0)
  record(withr::with_seed(
    seed,
    .update_scaled_expr_mat(
      x,
      .impute_fw_knn,
      work_scale = "log2",
      allow_logx = TRUE,
      .log_input = TRUE,
      by = by,
      k = k,
      rng.seed = seed,
      ...
    )
  ))
}


#' BPCA Imputation
#'
#' A wrapper around the [pcaMethods::pca()].
#' Impute missing values using Bayesian principal component analysis (BPCA).
#' BPCA combines an EM approach for PCA with a Bayesian model.
#' In standard PCA data far from the training set but close to the principal
#' subspace may have the same reconstruction error.
#' BPCA defines a likelihood function such that the likelihood for data far from
#' the training set is much lower, even if they are close to the principal subspace.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param seed Non-negative integer seed for random number generation. Default is
#'   `123`. The caller's random state is restored, including on errors.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_bpca <- function(x, by = NULL, ..., seed = 123) {
  record <- .pipeline_recorder(x)
  record({
    checkmate::assert_int(seed, lower = 0)
    withr::with_seed(
      seed,
      .update_scaled_expr_mat(
        x,
        .impute_bpca,
        work_scale = "log2",
        allow_logx = TRUE,
        .log_input = TRUE,
        by = by,
        ...
      )
    )
  })
}


#' PPCA Imputation
#'
#' A wrapper around the [pcaMethods::pca()].
#' Impute missing values using probabilistic principal component analysis (PPCA).
#' PPCA allows to perform PCA on incomplete data and may be used for missing value estimation.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param seed Non-negative integer seed for random number generation. Default is
#'   `123`. The caller's random state is restored, including on errors.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_ppca <- function(x, by = NULL, ..., seed = 123) {
  record <- .pipeline_recorder(x)
  record({
    checkmate::assert_int(seed, lower = 0)
    withr::with_seed(
      seed,
      .update_scaled_expr_mat(
        x,
        .impute_ppca,
        work_scale = "log2",
        allow_logx = TRUE,
        .log_input = TRUE,
        by = by,
        ...
      )
    )
  })
}


#' SVD Imputation
#'
#' A wrapper around the [pcaMethods::pca()].
#' Impute missing values using singular value decomposition (SVD) imputation.
#' SVD is a matrix factorization technique that factors a matrix into three matrices:
#' U, \eqn{\Sigma}, and V. SVD is used to find the best lower rank approximation of the original matrix.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param seed Non-negative integer seed for random number generation. Default is
#'   `123`. The caller's random state is restored, including on errors.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_svd <- function(x, by = NULL, ..., seed = 123) {
  record <- .pipeline_recorder(x)
  record({
    checkmate::assert_int(seed, lower = 0)
    withr::with_seed(
      seed,
      .update_scaled_expr_mat(
        x,
        .impute_svd,
        work_scale = "log2",
        allow_logx = TRUE,
        .log_input = TRUE,
        by = by,
        ...
      )
    )
  })
}


#' Minimum Probability Imputation
#'
#' Impute missing values using random draws from the left-censored
#' gaussian distribution. Missing values are imputed on the log2 intensity
#' scale and then transformed back to the original scale.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param q Quantile used to estimate the lower-intensity center for each
#'   sample. Default is `0.01`.
#' @param tune.sigma Non-negative multiplier for the standard deviation of the
#'   left-censored draw distribution. Default is `1`.
#' @param seed Non-negative integer seed for random number generation. Default is
#'   `123`. The caller's random state is restored, including on errors.
#' @param ... Reserved for backward compatibility. Extra arguments are not
#'   supported.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_min_prob <- function(
  x,
  by = NULL,
  q = 0.01,
  tune.sigma = 1,
  ...,
  seed = 123
) {
  record <- .pipeline_recorder(x)
  record({
    checkmate::assert_int(seed, lower = 0)
    withr::with_seed(
      seed,
      .update_scaled_expr_mat(
        x,
        .impute_min_prob,
        work_scale = "log2",
        .log_input = TRUE,
        by = by,
        q = q,
        tune.sigma = tune.sigma,
        ...
      )
    )
  })
}


#' MissForest Imputation
#'
#' A wrapper around the [missForest::missForest()].
#' Impute missing values using recursive running of random forests until convergence.
#' This is a non-parametric method and works for both MAR and MNAR missing data.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param seed Non-negative integer seed for random number generation. Default is
#'   `123`. The caller's random state is restored, including on errors.
#' @param ... Additional arguments to pass to `missForest::missForest()`.
#'
#' @return A container of the same class as `x`, with missing values imputed.
#' @seealso [data_scales] for working domains and supported input scales.
#' @export
impute_miss_forest <- function(x, by = NULL, seed = 123, ...) {
  record <- .pipeline_recorder(x)
  record({
    checkmate::assert_int(seed, lower = 0)
    withr::with_seed(
      seed,
      .update_scaled_expr_mat(
        x,
        .impute_miss_forest,
        allow_logx = TRUE,
        by = by,
        ...
      )
    )
  })
}

# ===== Implementation =====

.impute_zero <- function(mat) {
  mat[is.na(mat)] <- 0
  mat
}


.impute_sample_min <- function(mat) {
  for (j in seq_len(ncol(mat))) {
    col <- mat[, j]
    if (!all(is.na(col))) {
      col[is.na(col)] <- min(col, na.rm = TRUE)
    }
    mat[, j] <- col
  }
  mat
}

.impute_half_sample_min <- function(mat) {
  result <- .impute_sample_min(mat)
  result[is.na(mat)] <- result[is.na(mat)] / 2
  result
}

.impute_half_sample_min_log <- function(mat, .half_log = 1) {
  result <- .impute_sample_min(mat)
  result[is.na(mat)] <- result[is.na(mat)] - .half_log
  result
}


.impute_sw_knn <- function(mat, k = 5, ..., .log_input = FALSE) {
  rlang::check_installed("impute", reason = "to use `impute_sw_knn()`")
  normed <- if (.log_input) mat else log2(mat)
  normed <- impute::impute.knn(normed, k = k, ...)$data
  if (.log_input) normed else 2^normed
}


.impute_fw_knn <- function(mat, k = 5, ..., .log_input = FALSE) {
  rlang::check_installed("impute", reason = "to use `impute_fw_knn()`")
  normed <- if (.log_input) mat else log2(mat)
  normed <- t(impute::impute.knn(t(normed), k = k, ...)$data)
  if (.log_input) normed else 2^normed
}


.impute_bpca <- function(mat, ..., .log_input = FALSE) {
  rlang::check_installed("pcaMethods", reason = "to use `impute_bpca()`")
  .pca_methods_wrapper(mat, "bpca", ..., .log_input = .log_input)
}


.impute_ppca <- function(mat, ..., .log_input = FALSE) {
  rlang::check_installed("pcaMethods", reason = "to use `impute_ppca()`")
  .pca_methods_wrapper(mat, "ppca", ..., .log_input = .log_input)
}


.impute_svd <- function(mat, ..., .log_input = FALSE) {
  rlang::check_installed("pcaMethods", reason = "to use `impute_svd()`")
  .pca_methods_wrapper(mat, "svdImpute", ..., .log_input = .log_input)
}


.pca_methods_wrapper <- function(mat, method, ..., .log_input = FALSE) {
  normed <- if (.log_input) mat else log2(mat)
  normed <- withr::with_output_sink(
    tempfile(),
    pcaMethods::pca(normed, nPcs = 5, method = method, ...)@completeObs
  )
  if (.log_input) normed else 2^normed
}


.impute_min_prob <- function(
  mat,
  q = 0.01,
  tune.sigma = 1,
  ...,
  .log_input = FALSE
) {
  .check_impute_min_prob_dots(...)
  checkmate::assert_number(q, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_number(tune.sigma, lower = 0, finite = TRUE)

  if (!anyNA(mat)) {
    return(mat)
  }

  normed <- if (.log_input) mat else log2(mat)
  sample_centers <- .min_prob_sample_centers(normed, q)
  draw_sd <- .min_prob_draw_sd(normed, tune.sigma)

  imputed_cols <- purrr::map(seq_len(ncol(normed)), function(sample_idx) {
    sample_values <- normed[, sample_idx]
    missing_idx <- which(is.na(sample_values))

    if (length(missing_idx) > 0) {
      sample_draws <- stats::rnorm(
        nrow(normed),
        mean = sample_centers[sample_idx],
        sd = draw_sd
      )
      sample_values[missing_idx] <- sample_draws[missing_idx]
    }

    sample_values
  })

  imputed <- do.call(cbind, imputed_cols)
  dimnames(imputed) <- dimnames(mat)
  if (.log_input) imputed else 2^imputed
}


#' Check unsupported MinProb compatibility arguments
#'
#' @param ... Arguments reserved for backward compatibility.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
.check_impute_min_prob_dots <- function(...) {
  dots <- rlang::list2(...)

  if (length(dots) == 0) {
    return(invisible(NULL))
  }

  cli::cli_abort(c(
    "Unsupported argument passed to {.fn impute_min_prob}.",
    "i" = "Use {.arg q} and {.arg tune.sigma} directly; other arguments are not supported."
  ))
}


#' Estimate MinProb sample centers
#'
#' @param normed A log2-transformed intensity matrix.
#' @param q Quantile used to estimate the lower-intensity center.
#'
#' @return A numeric vector with one center per sample.
#' @noRd
.min_prob_sample_centers <- function(normed, q) {
  observed_values <- as.numeric(normed[!is.na(normed)])
  if (length(observed_values) == 0) {
    cli::cli_abort(
      "{.arg x} must contain at least one observed value for {.fn impute_min_prob}."
    )
  }

  global_center <- unname(stats::quantile(
    observed_values,
    probs = q,
    na.rm = TRUE
  ))
  centers <- purrr::map_dbl(
    seq_len(ncol(normed)),
    ~ unname(stats::quantile(normed[, .x], probs = q, na.rm = TRUE))
  )
  centers[is.na(centers)] <- global_center
  centers
}


#' Estimate MinProb draw standard deviation
#'
#' @param normed A log2-transformed intensity matrix.
#' @param tune.sigma Multiplier for the left-censored draw standard deviation.
#'
#' @return A scalar standard deviation for random draws.
#' @noRd
.min_prob_draw_sd <- function(normed, tune.sigma) {
  observed_fraction <- rowMeans(!is.na(normed))
  filtered <- normed[observed_fraction > 0.5, , drop = FALSE]
  feature_sds <- purrr::map_dbl(
    seq_len(nrow(filtered)),
    ~ stats::sd(filtered[.x, ], na.rm = TRUE)
  )
  draw_sd <- stats::median(feature_sds, na.rm = TRUE) * tune.sigma

  if (is.finite(draw_sd)) {
    return(draw_sd)
  }

  fallback_sd <- stats::sd(as.numeric(normed), na.rm = TRUE) * tune.sigma
  if (is.finite(fallback_sd)) {
    return(fallback_sd)
  }

  0
}


.impute_miss_forest <- function(mat, ...) {
  rlang::check_installed("missForest", reason = "to use `impute_miss_forest()`")
  missForest::missForest(mat, ...)$ximp
}
