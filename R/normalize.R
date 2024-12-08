# Normalization functions

# Naming convention: <method>_normalize
# Functions starting with a dot are implementation details, accepting a matrix
# and returning a normalized matrix. The public functions accept an experiment
# and return a new experiment.

# ---------- Interface ----------

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


#' Quantile Normalization
#'
#' Normalize the expression matrix by quantile normalization.
#' This method is used to remove technical variation between samples.
#' Proteomics data rarely uses this method, but it is common in microarray data.
#' See [wikipedia](https://en.wikipedia.org/wiki/Quantile_normalization)
#' for more information.
#'
#' @param exp An experiment object.
#'
#' @return An experiment object with the expression matrix normalized.
#' @export
quantile_normalize <- function(exp) {
  .normalize(exp, .quantile_normalize)
}


#' LoessF Normalization
#'
#' This function is a wrapper around [limma::normalizeCyclicLoess()] with
#' `method = "fast"`.
#' Each column is simply normalized to a reference array,
#' the reference array being the average of all the arrays.
#' See [this paper](https://doi.org/10.1093/bib/bbw095) for more information.
#' Also see [limma::normalizeCyclicLoess()].
#'
#' @param exp An experiment object.
#' @param ... Additional arguments to pass to [limma::normalizeCyclicLoess()].
#'
#' @return An experiment object with the expression matrix normalized.
#' @export
loessf_normalize <- function(exp, ...) {
  .normalize(exp, .loessf_normalize)
}


#' LoessCyc Normalization
#'
#' This function is a wrapper around [limma::normalizeCyclicLoess()] with
#' `method = "pairs"`.
#' Each pair of columns is normalized mutually to each other.
#' See [this paper](https://doi.org/10.1093/bib/bbw095) for more information.
#' Also see [limma::normalizeCyclicLoess()].
#'
#' @param exp An experiment object.
#' @param ... Additional arguments to pass to [limma::normalizeCyclicLoess()].
#'
#' @return An experiment object with the expression matrix normalized.
#' @export
loesscyc_normalize <- function(exp, ...) {
  .normalize(exp, .loesscyc_normalize)
}


# ---------- Implementation ----------

.normalize <- function(exp, f, ...) {
  new_expr_mat <- f(exp$expr_mat, ...)
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


.quantile_normalize <- function(mat) {
  mat_sort <- apply(mat, 2, sort)
  row_means <- rowMeans(mat_sort)
  mat_sort <- matrix(row_means, nrow = nrow(mat), ncol = ncol(mat))
  index_rank <- apply(mat, 2, order)
  normed_mat <- apply(index_rank, 2, function(idx) mat_sort[idx, 1])
  colnames(normed_mat) <- colnames(mat)
  rownames(normed_mat) <- rownames(mat)
  normed_mat
}


.loessf_normalize <- function(mat, ...) {
  log_mat <- log2(mat)
  normed <- limma::normalizeCyclicLoess(log_mat, method = "fast", ...)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}


.loesscyc_normalize <- function(mat, ...) {
  log_mat <- log2(mat)
  normed <- limma::normalizeCyclicLoess(log_mat, method = "pairs", ...)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}
