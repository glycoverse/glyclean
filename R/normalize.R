# Normalization functions

# Naming convention: normalize_<method>
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
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_median <- function(x) {
  UseMethod("normalize_median")
}

#' @rdname normalize_median
#' @export
normalize_median.glyexp_experiment <- function(x) {
  .dispatch_and_apply_by_group(x, .normalize_median, by = NULL)
}

#' @rdname normalize_median
#' @export
normalize_median.matrix <- function(x) {
  .dispatch_and_apply_by_group(x, .normalize_median, by = NULL)
}

#' @rdname normalize_median
#' @export
normalize_median.default <- function(x) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Absolute Median Normalization
#'
#' Normalize the expression matrix by dividing each column (sample) by the absolute
#' median of that column (NA ignored), so that the absolute median of each column is 1.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_median_abs <- function(x) {
  UseMethod("normalize_median_abs")
}

#' @rdname normalize_median_abs
#' @export
normalize_median_abs.glyexp_experiment <- function(x) {
  .dispatch_and_apply_by_group(x, .normalize_median_abs, by = NULL)
}

#' @rdname normalize_median_abs
#' @export
normalize_median_abs.matrix <- function(x) {
  .dispatch_and_apply_by_group(x, .normalize_median_abs, by = NULL)
}

#' @rdname normalize_median_abs
#' @export
normalize_median_abs.default <- function(x) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Total Area Normalization
#'
#' Normalize the expression matrix by dividing each column (sample) by the sum of
#' that column, so that the sum of each column is 1.
#' This is the most common normalization method for glycomics data.
#' It removes the bias introduced by total glycan abundance.
#' However, it results in compositional data, which may result in unrealistic
#' downstream analysis results.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_total_area <- function(x) {
  UseMethod("normalize_total_area")
}

#' @rdname normalize_total_area
#' @export
normalize_total_area.glyexp_experiment <- function(x) {
  .dispatch_and_apply_by_group(x, .normalize_total_area, by = NULL)
}

#' @rdname normalize_total_area
#' @export
normalize_total_area.matrix <- function(x) {
  .dispatch_and_apply_by_group(x, .normalize_total_area, by = NULL)
}

#' @rdname normalize_total_area
#' @export
normalize_total_area.default <- function(x) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
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
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#' @param ... Additional arguments to pass to [limma::normalizeQuantiles()].
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_quantile <- function(x, by = NULL, ...) {
  UseMethod("normalize_quantile")
}

#' @rdname normalize_quantile
#' @export
normalize_quantile.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_quantile, by = by, ...)
}

#' @rdname normalize_quantile
#' @export
normalize_quantile.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_quantile, by = by, ...)
}

#' @rdname normalize_quantile
#' @export
normalize_quantile.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
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
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#' @param ... Additional arguments to pass to [limma::normalizeCyclicLoess()].
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_loessf <- function(x, by = NULL, ...) {
  UseMethod("normalize_loessf")
}

#' @rdname normalize_loessf
#' @export
normalize_loessf.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_loessf, by = by, ...)
}

#' @rdname normalize_loessf
#' @export
normalize_loessf.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_loessf, by = by, ...)
}

#' @rdname normalize_loessf
#' @export
normalize_loessf.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' LoessCyc Normalization
#'
#' This function is a wrapper around [limma::normalizeCyclicLoess()] with
#' `method = "pairs"`.
#' Each pair of columns is normalized mutually to each other.
#' See [this paper](https://doi.org/10.1093/bib/bbw095) for more information.
#' Also see [limma::normalizeCyclicLoess()].
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#' @param ... Additional arguments to pass to [limma::normalizeCyclicLoess()].
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_loesscyc <- function(x, by = NULL, ...) {
  UseMethod("normalize_loesscyc")
}

#' @rdname normalize_loesscyc
#' @export
normalize_loesscyc.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_loesscyc, by = by, ...)
}

#' @rdname normalize_loesscyc
#' @export
normalize_loesscyc.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_loesscyc, by = by, ...)
}

#' @rdname normalize_loesscyc
#' @export
normalize_loesscyc.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
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
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#' @param ... Additional arguments to pass to [limma::normalizeVSN()].
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_vsn <- function(x, by = NULL, ...) {
  UseMethod("normalize_vsn")
}

#' @rdname normalize_vsn
#' @export
normalize_vsn.glyexp_experiment <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_vsn, by = by, ...)
}

#' @rdname normalize_vsn
#' @export
normalize_vsn.matrix <- function(x, by = NULL, ...) {
  .dispatch_and_apply_by_group(x, .normalize_vsn, by = by, ...)
}

#' @rdname normalize_vsn
#' @export
normalize_vsn.default <- function(x, by = NULL, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Median Quotient Normalization
#'
#' This approach is based on the calculation of the dilution factor of each
#' sample with respect to a reference sample. Here, the reference sample
#' was calculated as the median value of each glycan's abundance across all
#' measured samples. For each sample, a vector of quotients was then obtained
#' by dividing each glycan measure by the corresponding value in the reference
#' sample. The median of these quotients was then used as the sample's dilution
#' factor, and the original sample values were subsequently divided by that value.
#' The underlying assumption is that the diﬀerent intensities observed across
#' individuals are imputable to diﬀerent amounts of the biological material
#' in the collected samples.
#' See [this paper](https://dx.doi.org/10.1021/ac051632c) for more information.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_median_quotient <- function(x, by = NULL) {
  UseMethod("normalize_median_quotient")
}

#' @rdname normalize_median_quotient
#' @export
normalize_median_quotient.glyexp_experiment <- function(x, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_median_quotient, by = by)
}

#' @rdname normalize_median_quotient
#' @export
normalize_median_quotient.matrix <- function(x, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_median_quotient, by = by)
}

#' @rdname normalize_median_quotient
#' @export
normalize_median_quotient.default <- function(x, by = NULL) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Robust Linear Regression Normalization
#'
#' This method is based on robust linear regression. The reference sample is
#' calculated as the median value of each variable's abundance across all
#' measured samples. For each sample, a robust linear regression model is
#' fitted to the sample's abundance values against the reference sample's
#' abundance values. The fitted model is then used to normalize the sample's
#' abundance values. The underlying assumption is that the diﬀerent intensities
#' observed across individuals are imputable to diﬀerent amounts of the biological
#' material in the collected samples.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_rlr <- function(x, by = NULL) {
  UseMethod("normalize_rlr")
}

#' @rdname normalize_rlr
#' @export
normalize_rlr.glyexp_experiment <- function(x, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_rlr, by = by)
}

#' @rdname normalize_rlr
#' @export
normalize_rlr.matrix <- function(x, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_rlr, by = by)
}

#' @rdname normalize_rlr
#' @export
normalize_rlr.default <- function(x, by = NULL) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Robust Linear Regression with Median Adjustment Normalization
#'
#' This method is based on robust linear regression with median adjustment.
#' First, the median of each variable's abundance across all measured samples
#' is subtracted from the sample's abundance values. Then, it's like the
#' [normalize_rlr()] method.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_rlrma <- function(x, by = NULL) {
  UseMethod("normalize_rlrma")
}

#' @rdname normalize_rlrma
#' @export
normalize_rlrma.glyexp_experiment <- function(x, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_rlrma, by = by)
}

#' @rdname normalize_rlrma
#' @export
normalize_rlrma.matrix <- function(x, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_rlrma, by = by)
}

#' @rdname normalize_rlrma
#' @export
normalize_rlrma.default <- function(x, by = NULL) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Robust Linear Regression with Median Adjustment and Cyclic Normalization
#'
#' This method is based on robust linear regression with median adjustment
#' and cyclic normalization. The method is applied iteratively to each pair
#' of samples. For each pair of samples, the median of the differences between
#' the two samples is calculated. A robust linear regression model is fitted
#' to the differences against the averages of the two samples. The fitted model
#' is then used to normalize the two samples. The process is repeated for a
#' number of iterations.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param n_iter The number of iterations to perform. Default is 3.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample. Optional.
#'   If provided, the normalization will be performed within each group.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with normalized expression matrix.
#'   If `x` is a matrix, returns a normalized matrix.
#' @export
normalize_rlrmacyc <- function(x, n_iter = 3, by = NULL) {
  UseMethod("normalize_rlrmacyc")
}

#' @rdname normalize_rlrmacyc
#' @export
normalize_rlrmacyc.glyexp_experiment <- function(x, n_iter = 3, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_rlrmacyc, by = by, n_iter = n_iter)
}

#' @rdname normalize_rlrmacyc
#' @export
normalize_rlrmacyc.matrix <- function(x, n_iter = 3, by = NULL) {
  .dispatch_and_apply_by_group(x, .normalize_rlrmacyc, by = by, n_iter = n_iter)
}

#' @rdname normalize_rlrmacyc
#' @export
normalize_rlrmacyc.default <- function(x, n_iter = 3, by = NULL) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


# ---------- Implementation ----------
.normalize_median <- function(mat) {
  normed <- limma::normalizeMedianValues(mat)
  if (any(colMeans(is.nan(normed)) == 1)) {
    cli::cli_warn("Some samples have median values of {.val 0}, producing all NaNs after normalization.")
  }
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  normed
}


.normalize_median_abs <- function(mat) {
  normed <- limma::normalizeMedianAbsValues(mat)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  normed
}


.normalize_total_area <- function(mat) {
  col_sums <- colSums(mat, na.rm = TRUE)
  sweep(mat, 2, col_sums, "/")
}


.normalize_quantile <- function(mat, ...) {
  normed <- limma::normalizeQuantiles(mat, ...)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  normed
}


.normalize_loessf <- function(mat, ...) {
  log_mat <- log2(mat)
  normed <- limma::normalizeCyclicLoess(log_mat, method = "fast", ...)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}


.normalize_loesscyc <- function(mat, ...) {
  log_mat <- log2(mat)
  normed <- limma::normalizeCyclicLoess(log_mat, method = "pairs", ...)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}


.normalize_vsn <- function(mat, ...) {
  rlang::check_installed("vsn", reason = "to use `normalize_vsn()`")
  if (nrow(mat) < 42) {
    rlang::abort("The number of variables should be at least 42 for `normalize_vsn()`")
  }
  suppressMessages(normed <- limma::normalizeVSN(mat, ...))
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  normed
}


.normalize_median_quotient <- function(mat) {
  # The reference sample was calculated as the median value of
  # each glycan's abundance across all measured samples.
  ref_sample <- matrixStats::rowMedians(mat, na.rm = TRUE, useNames = TRUE)

  # For each sample, a vector of quotients was then obtained by
  # dividing each glycan measure by the corresponding value in the reference sample.
  # The median of these quotients was then used as the sample's dilution factor.
  dilution_factor <- apply(mat / ref_sample, 2, stats::median, na.rm = TRUE)

  # The original sample values were subsequently divided by that value.
  t(t(mat) / dilution_factor)
}


.normalize_rlr <- function(mat) {
  rlang::check_installed("MASS", reason = "to use `normalize_rlr()`")
  normed <- log2(mat)
  ref_sample <- matrixStats::rowMedians(normed, na.rm = TRUE, useNames = TRUE)
  for (i in seq_len(ncol(normed))) {
    sample <- normed[, i]
    lr_fit <- suppressWarnings(MASS::rlm(
      as.matrix(sample) ~ ref_sample,
      na.action = stats::na.exclude,
      maxit = 50
    ))
    intercept <- lr_fit$coefficients[1]
    slope <- lr_fit$coefficients[2]
    normed[, i] <- (sample - intercept) / slope
  }
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}


.normalize_rlrma <- function(mat) {
  rlang::check_installed("MASS", reason = "to use `normalize_rlrma()`")
  normed <- log2(mat)
  ref_sample <- matrixStats::rowMedians(normed, na.rm = TRUE, useNames = TRUE)
  for (i in seq_len(ncol(normed))) {
    sample <- normed[, i]
    m <- sample - ref_sample  # MA transformation
    lr_fit <- suppressWarnings(MASS::rlm(m ~ ref_sample, na.action = stats::na.exclude, maxit = 50))
    fit_values <- stats::predict(lr_fit)
    normed[, i] <- m - fit_values
  }
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}


.normalize_rlrmacyc <- function(mat, n_iter = 3) {
  rlang::check_installed("MASS", reason = "to use `normalize_rlrmacyc()`")
  normed <- log2(mat)
  for (k in seq_len(n_iter)) {
    for (j in seq_len(ncol(normed))) {
      if (j == ncol(normed)) {
        break
      }
      for (i in seq(j + 1, ncol(normed))) {
        sample1 <- normed[, i]
        sample2 <- normed[, j]
        m <- sample1 - sample2
        a <- (sample1 + sample2) / 2
        fit <- suppressWarnings(MASS::rlm(m ~ a, na.action = stats::na.exclude, maxit = 50))
        fit_values <- stats::predict(fit)
        normed[, i] <- sample1 - fit_values / 2
        normed[, j] <- sample2 + fit_values / 2
      }
    }
  }
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2 ^ normed
}
