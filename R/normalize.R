# Normalization functions

# Naming convention: <method>_normalize
# Functions starting with a dot are implementation details, accepting a matrix
# and returning a normalized matrix. The public functions accept an experiment
# and return a new experiment.


#' Median Normalization
#'
#' Normalize the expression matrix by dividing each column (sample) by the median
#' of that column (NA ignored), so that the median of each column is 1.
#' This is the most common normalization method for proteomics data.
#' It effectively and robustly removes the bias introduced by total protein abundance,
#' and removes batch effects in part.
#'
#' @param exp An experiment object.
#'
#' @return An experiment object with the expression matrix normalized.
#' @export
median_normalize <- function(exp) {
  .normalize(exp, .median_normalize)
}


#' Total Abundance Normalization
#'
#' Normalize the expression matrix by dividing each column (sample) by the sum of
#' that column, so that the sum of each column is 1.
#' This is the most common normalization method for glycomics data.
#' It removes the bias introduced by total glycan abundance.
#' However, it results in compositional data, which may result in unrealistic
#' downstream analysis results.
#'
#' @param exp An experiment object.
#'
#' @return An experiment object with the expression matrix normalized.
#' @export
ta_normalize <- function(exp) {
  .normalize(exp, .ta_normalize)
}


.normalize <- function(exp, f) {
  new_expr_mat <- f(exp$expr_mat)
  exp$expr_mat <- new_expr_mat
  exp
}


.median_normalize <- function(mat) {
  col_medians <- apply(mat, 2, stats::median, na.rm = TRUE)
  sweep(mat, 2, col_medians, "/")
}


.ta_normalize <- function(mat) {
  col_sums <- colSums(mat, na.rm = TRUE)
  sweep(mat, 2, col_sums, "/")
}
