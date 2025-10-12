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
#' A wrapper around the [imputeLCMD::impute.MinProb()].
#' Impute missing values using random draws from the left-censored
#' gaussian distribution.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Used for grouping when imputing missing values.
#' @param ... Additional arguments to pass to `imputeLCMD::impute.MinProb()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_min_prob <- function(x, by = NULL, ...) {
  UseMethod("impute_min_prob")
}

#' @rdname impute_min_prob
#' @export
impute_min_prob.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_min_prob, by = by, ...)
}

#' @rdname impute_min_prob
#' @export
impute_min_prob.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_min_prob, by = by, ...)
}

#' @rdname impute_min_prob
#' @export
impute_min_prob.default <- function(x, by = NULL, ...) {
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
#' @param ... Additional arguments to pass to `missForest::missForest()`.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with missing values imputed.
#'   If `x` is a matrix, returns a matrix with missing values imputed.
#' @export
impute_miss_forest <- function(x, by = NULL, ...) {
  UseMethod("impute_miss_forest")
}

#' @rdname impute_miss_forest
#' @export
impute_miss_forest.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_miss_forest, by = by, ...)
}

#' @rdname impute_miss_forest
#' @export
impute_miss_forest.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .impute_miss_forest, by = by, ...)
}

#' @rdname impute_miss_forest
#' @export
impute_miss_forest.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


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
  2 ^ normed
}


.impute_fw_knn <- function(mat, k = 5, ...) {
  rlang::check_installed("impute", reason = "to use `impute_fw_knn()`")
  normed <- log2(mat)
  normed <- t(impute::impute.knn(t(normed), k = k, ...)$data)
  2 ^ normed
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
  2 ^ normed
}


.impute_min_prob <- function(mat, ...) {
  rlang::check_installed("imputeLCMD", reason = "to use `impute_min_prob()`")
  normed <- log2(mat)
  # Use withr::with_output_sink to silence the output from imputeLCMD::impute.MinProb
  normed <- withr::with_output_sink(
    tempfile(),
    imputeLCMD::impute.MinProb(normed, ...)
  )
  2 ^ normed
}


.impute_miss_forest <- function(mat, ...) {
  rlang::check_installed("missForest", reason = "to use `impute_miss_forest()`")
  missForest::missForest(mat, ...)$ximp
}
