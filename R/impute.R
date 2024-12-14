#' Zero imputation
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


#' Sample minimum imputation
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


#' Half sample minimum imputation
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
