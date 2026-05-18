#' Zero Imputation
#'
#' Impute missing values in an expression matrix by replacing them with zeros.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_zero <- function(x) {
  UseMethod("impute_zero")
}

#' @rdname impute_zero
#' @export
impute_zero.glyexp_experiment <- function(x) {
  .dispatch_and_apply_by_group(x, .impute_zero, by = NULL)
}

#' @rdname impute_zero
#' @export
impute_zero.matrix <- function(x) {
  .dispatch_and_apply_by_group(x, .impute_zero, by = NULL)
}

#' @rdname impute_zero
#' @export
impute_zero.default <- function(x) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Sample Minimum Imputation
#'
#' Impute missing values with the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' See also [impute_half_sample_min()].
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_sample_min <- function(x) {
  UseMethod("impute_sample_min")
}

#' @rdname impute_sample_min
#' @export
impute_sample_min.glyexp_experiment <- function(x) {
  .dispatch_and_apply_by_group(x, .impute_sample_min, by = NULL)
}

#' @rdname impute_sample_min
#' @export
impute_sample_min.matrix <- function(x) {
  .dispatch_and_apply_by_group(x, .impute_sample_min, by = NULL)
}

#' @rdname impute_sample_min
#' @export
impute_sample_min.default <- function(x) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Half Sample Minimum Imputation
#'
#' Impute missing values with half of the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' Compared to [impute_sample_min()], this method is more conservative.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_half_sample_min <- function(x) {
  UseMethod("impute_half_sample_min")
}

#' @rdname impute_half_sample_min
#' @export
impute_half_sample_min.glyexp_experiment <- function(x) {
  .dispatch_and_apply_by_group(x, .impute_half_sample_min, by = NULL)
}

#' @rdname impute_half_sample_min
#' @export
impute_half_sample_min.matrix <- function(x) {
  .dispatch_and_apply_by_group(x, .impute_half_sample_min, by = NULL)
}

#' @rdname impute_half_sample_min
#' @export
impute_half_sample_min.default <- function(x) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
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
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param k The number of nearest neighbors to consider.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param ... Additional arguments to pass to `impute::impute.knn()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_sw_knn <- function(x, k = 5, by = NULL, ...) {
  UseMethod("impute_sw_knn")
}

#' @rdname impute_sw_knn
#' @export
impute_sw_knn.glyexp_experiment <- function(x, k = 5, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_sw_knn, k = k, by = by, ...)
}

#' @rdname impute_sw_knn
#' @export
impute_sw_knn.matrix <- function(x, k = 5, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_sw_knn, k = k, by = by, ...)
}

#' @rdname impute_sw_knn
#' @export
impute_sw_knn.default <- function(x, k = 5, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Feature-wise KNN Imputation
#'
#' A wrapper around the [impute::impute.knn()].
#' Impute missing values with values from the k-nearest neighbors of the
#' corresponding feature.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param k The number of nearest neighbors to consider.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param ... Additional arguments to pass to `impute::impute.knn()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_fw_knn <- function(x, k = 5, by = NULL, ...) {
  UseMethod("impute_fw_knn")
}

#' @rdname impute_fw_knn
#' @export
impute_fw_knn.glyexp_experiment <- function(x, k = 5, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_fw_knn, k = k, by = by, ...)
}

#' @rdname impute_fw_knn
#' @export
impute_fw_knn.matrix <- function(x, k = 5, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_fw_knn, k = k, by = by, ...)
}

#' @rdname impute_fw_knn
#' @export
impute_fw_knn.default <- function(x, k = 5, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
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
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_bpca <- function(x, by = NULL, ...) {
  UseMethod("impute_bpca")
}

#' @rdname impute_bpca
#' @export
impute_bpca.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_bpca, by = by, ...)
}

#' @rdname impute_bpca
#' @export
impute_bpca.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_bpca, by = by, ...)
}

#' @rdname impute_bpca
#' @export
impute_bpca.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' PPCA Imputation
#'
#' A wrapper around the [pcaMethods::pca()].
#' Impute missing values using probabilistic principal component analysis (PPCA).
#' PPCA allows to perform PCA on incomplete data and may be used for missing value estimation.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_ppca <- function(x, by = NULL, ...) {
  UseMethod("impute_ppca")
}

#' @rdname impute_ppca
#' @export
impute_ppca.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_ppca, by = by, ...)
}

#' @rdname impute_ppca
#' @export
impute_ppca.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_ppca, by = by, ...)
}

#' @rdname impute_ppca
#' @export
impute_ppca.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' SVD Imputation
#'
#' A wrapper around the [pcaMethods::pca()].
#' Impute missing values using singular value decomposition (SVD) imputation.
#' SVD is a matrix factorization technique that factors a matrix into three matrices:
#' U, Σ, and V. SVD is used to find the best lower rank approximation of the original matrix.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_svd <- function(x, by = NULL, ...) {
  UseMethod("impute_svd")
}

#' @rdname impute_svd
#' @export
impute_svd.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_svd, by = by, ...)
}

#' @rdname impute_svd
#' @export
impute_svd.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_svd, by = by, ...)
}

#' @rdname impute_svd
#' @export
impute_svd.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Minimum Probability Imputation
#'
#' Impute missing values using random draws from the left-censored
#' gaussian distribution. Missing values are imputed on the log2 intensity
#' scale and then transformed back to the original scale.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param q Quantile used to estimate the lower-intensity center for each
#'   sample. Default is `0.01`.
#' @param tune.sigma Non-negative multiplier for the standard deviation of the
#'   left-censored draw distribution. Default is `1`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_min_prob <- function(x, by = NULL, q = 0.01, tune.sigma = 1) {
  UseMethod("impute_min_prob")
}

#' @rdname impute_min_prob
#' @export
impute_min_prob.glyexp_experiment <- function(
  x,
  by = NULL,
  q = 0.01,
  tune.sigma = 1
) {
  .dispatch_and_apply_by_group(
    x,
    .impute_min_prob,
    by = by,
    q = q,
    tune.sigma = tune.sigma
  )
}

#' @rdname impute_min_prob
#' @export
impute_min_prob.matrix <- function(x, by = NULL, q = 0.01, tune.sigma = 1) {
  .dispatch_and_apply_by_group(
    x,
    .impute_min_prob,
    by = by,
    q = q,
    tune.sigma = tune.sigma
  )
}

#' @rdname impute_min_prob
#' @export
impute_min_prob.default <- function(x, by = NULL, q = 0.01, tune.sigma = 1) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' MissForest Imputation
#'
#' A wrapper around the [missForest::missForest()].
#' Impute missing values using recursive running of random forests until convergence.
#' This is a non-parametric method and works for both MAR and MNAR missing data.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param seed Integer seed for random number generation. Default is 123.
#' @param ... Additional arguments to pass to `missForest::missForest()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_miss_forest <- function(x, by = NULL, seed = 123, ...) {
  UseMethod("impute_miss_forest")
}

#' @rdname impute_miss_forest
#' @export
impute_miss_forest.glyexp_experiment <- function(
  x,
  by = NULL,
  seed = 123,
  ...
) {
  .dispatch_and_apply_by_group(
    x,
    .impute_miss_forest,
    by = by,
    seed = seed,
    ...
  )
}

#' @rdname impute_miss_forest
#' @export
impute_miss_forest.matrix <- function(x, by = NULL, seed = 123, ...) {
  .dispatch_and_apply_by_group(
    x,
    .impute_miss_forest,
    by = by,
    seed = seed,
    ...
  )
}

#' @rdname impute_miss_forest
#' @export
impute_miss_forest.default <- function(x, by = NULL, seed = 123, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}

# ===== Implementation =====

.impute_zero <- function(mat) {
  mat[is.na(mat)] <- 0
  mat
}


.impute_sample_min <- function(mat) {
  apply(mat, 2, function(col) {
    col[is.na(col)] <- min(col, na.rm = TRUE)
    col
  })
}


.impute_half_sample_min <- function(mat) {
  apply(mat, 2, function(col) {
    col[is.na(col)] <- min(col, na.rm = TRUE) / 2
    col
  })
}


.impute_sw_knn <- function(mat, k = 5, ...) {
  rlang::check_installed("impute", reason = "to use `impute_sw_knn()`")
  normed <- log2(mat)
  normed <- impute::impute.knn(normed, k = k, ...)$data
  2^normed
}


.impute_fw_knn <- function(mat, k = 5, ...) {
  rlang::check_installed("impute", reason = "to use `impute_fw_knn()`")
  normed <- log2(mat)
  normed <- t(impute::impute.knn(t(normed), k = k, ...)$data)
  2^normed
}


.impute_bpca <- function(mat, ...) {
  rlang::check_installed("pcaMethods", reason = "to use `impute_bpca()`")
  .pca_methods_wrapper(mat, "bpca", ...)
}


.impute_ppca <- function(mat, ...) {
  rlang::check_installed("pcaMethods", reason = "to use `impute_ppca()`")
  .pca_methods_wrapper(mat, "ppca", ...)
}


.impute_svd <- function(mat, ...) {
  rlang::check_installed("pcaMethods", reason = "to use `impute_svd()`")
  .pca_methods_wrapper(mat, "svdImpute", ...)
}


.pca_methods_wrapper <- function(mat, method, ...) {
  normed <- log2(mat)
  normed <- withr::with_output_sink(
    tempfile(),
    pcaMethods::pca(normed, nPcs = 5, method = method, ...)@completeObs
  )
  2^normed
}


.impute_min_prob <- function(mat, q = 0.01, tune.sigma = 1) {
  checkmate::assert_number(q, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_number(tune.sigma, lower = 0, finite = TRUE)

  if (!anyNA(mat)) {
    return(mat)
  }

  normed <- log2(mat)
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
  2^imputed
}


.min_prob_sample_centers <- function(normed, q) {
  purrr::map_dbl(
    seq_len(ncol(normed)),
    ~ unname(stats::quantile(normed[, .x], probs = q, na.rm = TRUE))
  )
}


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


.impute_miss_forest <- function(mat, seed, ...) {
  rlang::check_installed("missForest", reason = "to use `impute_miss_forest()`")
  withr::with_seed(seed, missForest::missForest(mat, ...)$ximp)
}
