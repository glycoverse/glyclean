#' Zero Imputation
#'
#' Impute missing values in an expression matrix by replacing them with zeros.
#'
#' @param exp An expression matrix.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_zero <- function(exp) {
  .update_expr_mat(exp, .impute_zero, by = NULL)
}


#' Sample Minimum Imputation
#'
#' Impute missing values with the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' See also [impute_half_sample_min()].
#'
#' @param exp An expression matrix.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_sample_min <- function(exp) {
  .update_expr_mat(exp, .impute_sample_min, by = NULL)
}


#' Half Sample Minimum Imputation
#'
#' Impute missing values with half of the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' Compared to [impute_sample_min()], this method is more conservative.
#'
#' @param exp An expression matrix.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_half_sample_min <- function(exp) {
  .update_expr_mat(exp, .impute_half_sample_min, by = NULL)
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
#' @param exp An expression matrix.
#' @param k The number of nearest neighbors to consider.
#' @param by A grouping variable to consider when imputing missing values.
#' This variable should be a column in the sample information table.
#' @param ... Additional arguments to pass to `impute::impute.knn()`.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_sw_knn <- function(exp, k = 5, by = NULL, ...) {
  .update_expr_mat(exp, .impute_sw_knn, k = k, by = by, ...)
}


#' Feature-wise KNN Imputation
#'
#' A wrapper around the [impute::impute.knn()].
#' Impute missing values with values from the k-nearest neighbors of the
#' corresponding feature.
#'
#' @param exp An expression matrix.
#' @param k The number of nearest neighbors to consider.
#' @param by A grouping variable to consider when imputing missing values.
#' This variable should be a column in the variable information table.
#' @param ... Additional arguments to pass to `impute::impute.knn()`.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_fw_knn <- function(exp, k = 5, by = NULL, ...) {
  .update_expr_mat(exp, .impute_fw_knn, k = k, by = by, ...)
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
#' @param exp An expression matrix.
#' @param by A grouping variable to consider when imputing missing values.
#' This variable should be a column in the sample information table.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_bpca <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .impute_bpca, by = by, ...)
}


#' PPCA Imputation
#'
#' A wrapper around the [pcaMethods::pca()].
#' Impute missing values using probabilistic principal component analysis (PPCA).
#' PPCA allows to perform PCA on incomplete data and may be used for missing value estimation.
#'
#' @param exp An expression matrix.
#' @param by A grouping variable to consider when imputing missing values.
#' This variable should be a column in the sample information table.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_ppca <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .impute_ppca, by = by, ...)
}


#' SVD Imputation
#'
#' A wrapper around the [pcaMethods::pca()].
#' Impute missing values using singular value decomposition (SVD) imputation.
#' SVD is a matrix factorization technique that factors a matrix into three matrices:
#' U, Σ, and V. SVD is used to find the best lower rank approximation of the original matrix.
#'
#' @param exp An expression matrix.
#' @param by A grouping variable to consider when imputing missing values.
#' This variable should be a column in the sample information table.
#' @param ... Additional arguments to pass to `pcaMethods::pca()`.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_svd <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .impute_svd, by = by, ...)
}


#' Minimum Probability Imputation
#'
#' A wrapper around the [imputeLCMD::impute.MinProb()].
#' Impute missing values using random draws from the left-censored
#' gaussian distribution.
#'
#' @param exp An expression matrix.
#' @param by A grouping variable to consider when imputing missing values.
#' This variable should be a column in the sample information table.
#' @param ... Additional arguments to pass to `imputeLCMD::impute.MinProb()`.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_min_prob <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .impute_min_prob, by = by, ...)
}


#' MissForest Imputation
#'
#' A wrapper around the [missForest::missForest()].
#' Impute missing values using recursive running of random forests until convergence.
#' This is a non-parametric method and works for both MAR and MNAR missing data.
#'
#' @param exp An expression matrix.
#' @param by A grouping variable to consider when imputing missing values.
#' This variable should be a column in the sample information table.
#' @param ... Additional arguments to pass to `missForest::missForest()`.
#'
#' @return An expression matrix with missing values imputed.
#' @export
impute_miss_forest <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .impute_miss_forest, by = by, ...)
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
  normed <- imputeLCMD::impute.MinProb(normed, ...)
  2 ^ normed
}


.impute_miss_forest <- function(mat, ...) {
  rlang::check_installed("missForest", reason = "to use `impute_miss_forest()`")
  missForest::missForest(mat, ...)$ximp
}
