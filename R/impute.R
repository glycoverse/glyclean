#' Zero Imputation
#'
#' Impute missing values in an expression matrix by replacing them with zeros.
#'
#' @param exp An expression matrix.
#'
#' @return An expression matrix with missing values imputed.
#' @export
zero_impute <- function(exp) {
  .update_expr_mat(exp, .zero_impute, by = NULL)
}


#' Sample Minimum Imputation
#'
#' Impute missing values with the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' See also [half_sample_min_impute()].
#'
#' @param exp An expression matrix.
#'
#' @return An expression matrix with missing values imputed.
#' @export
sample_min_impute <- function(exp) {
  .update_expr_mat(exp, .sample_min_impute, by = NULL)
}


#' Half Sample Minimum Imputation
#'
#' Impute missing values with half of the minimum value of the corresponding sample.
#' This method assumes that missing values are MCAR,
#' i.e. missing values are induced by an ion below the detection limit.
#' Compared to [sample_min_impute()], this method is more conservative.
#'
#' @param exp An expression matrix.
#'
#' @return An expression matrix with missing values imputed.
#' @export
half_sample_min_impute <- function(exp) {
  .update_expr_mat(exp, .half_sample_min_impute, by = NULL)
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
sw_knn_impute <- function(exp, k = 5, by = NULL, ...) {
  .update_expr_mat(exp, .sw_knn_impute, k = k, by = by, ...)
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
fw_knn_impute <- function(exp, k = 5, by = NULL, ...) {
  .update_expr_mat(exp, .fw_knn_impute, k = k, by = by, ...)
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
bpca_impute <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .bpca_impute, by = by, ...)
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
ppca_impute <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .ppca_impute, by = by, ...)
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
svd_impute <- function(exp, by = NULL, ...) {
  .update_expr_mat(exp, .svd_impute, by = by, ...)
}


.zero_impute <- function(mat) {
  mat[is.na(mat)] <- 0
  mat
}


.sample_min_impute <- function(mat) {
  apply(mat, 2, function(col) {
    tidyr::replace_na(col, min(col, na.rm = TRUE))
  })
}


.half_sample_min_impute <- function(mat) {
  apply(mat, 2, function(col) {
    tidyr::replace_na(col, min(col, na.rm = TRUE) / 2)
  })
}


.sw_knn_impute <- function(mat, k = 5, ...) {
  normed <- log2(mat)
  normed <- impute::impute.knn(normed, k = k, ...)$data
  2 ^ normed
}


.fw_knn_impute <- function(mat, k = 5, ...) {
  normed <- log2(mat)
  normed <- t(impute::impute.knn(t(normed), k = k, ...)$data)
  2 ^ normed
}


.bpca_impute <- function(mat, ...) {
  .pca_methods_wrapper(mat, "bpca", ...)
}


.ppca_impute <- function(mat, ...) {
  .pca_methods_wrapper(mat, "ppca", ...)
}


.svd_impute <- function(mat, ...) {
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
