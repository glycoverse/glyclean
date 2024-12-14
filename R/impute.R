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
  impute::impute.knn(mat, k = k, ...)$data
}


.fw_knn_impute <- function(mat, k = 5, ...) {
  t(impute::impute.knn(t(mat), k = k, ...)$data)
}
