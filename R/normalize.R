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


#' Absolute Median Normalization
#'
#' Normalize the expression matrix by dividing each column (sample) by the absolute
#' median of that column (NA ignored), so that the absolute median of each column is 1.
#'
#' @param exp An experiment object.
#'
#' @return An experiment object with the expression matrix normalized.
#' @export
median_abs_normalize <- function(exp) {
  .normalize(exp, .median_abs_normalize)
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
#' This function is a wrapper around [limma::normalizeQuantiles()].
#' It normalizes the expression matrix by quantile normalization.
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


#' Variance Stabilizing Normalization
#'
#' This function is a wrapper around [limma::normalizeVSN()].
#' Evidence proved that this method performs well in reducing noise and
#' boosting differential expression detection.
#' Log-transformation is not needed for downstream statistical analysis,
#' for this normalization method performs a log-like transformation internally.
#' Due to the same reason, fold change estimates will be severely distorted.
#' Please use this method with caution.
#' See [this paper](https://doi.org/10.1093/bib/bbw095) for more information.
#'
#' @details
#' At least 42 variables should be provided for this method.
#' This follows the convention of the `vsn` package.
#'
#' @param exp An experiment object.
#' @param ... Additional arguments to pass to [limma::normalizeVSN()].
#'
#' @return An experiment object with the expression matrix normalized.
#' @export
vsn_normalize <- function(exp, ...) {
  .normalize(exp, .vsn_normalize)
}


# ---------- Implementation ----------

.normalize <- function(exp, f, ...) {
  new_expr_mat <- f(exp$expr_mat, ...)
  exp$expr_mat <- new_expr_mat
  exp
}


.median_normalize <- function(mat) {
  log_mat <- log2(mat)
  normed <- limma::normalizeMedianValues(log_mat)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}


.median_abs_normalize <- function(mat) {
  log_mat <- log2(mat)
  normed <- limma::normalizeMedianAbsValues(log_mat)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}


.ta_normalize <- function(mat) {
  col_sums <- colSums(mat, na.rm = TRUE)
  sweep(mat, 2, col_sums, "/")
}


.quantile_normalize <- function(mat) {
  normed <- limma::normalizeQuantiles(mat)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  normed
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


.vsn_normalize <- function(mat, ...) {
  rlang::check_installed("vsn", reason = "to use `vsn_normalize()`")
  if (nrow(mat) < 42) {
    rlang::abort("The number of variables should be at least 42 for `vsn_normalize()`")
  }
  suppressMessages(normed <- limma::normalizeVSN(mat, ...))
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  normed
}
