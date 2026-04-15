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


#' Centered Log-Ratio Normalization
#'
#' This function implements a glycoWork-compatible CLR preprocessing strategy.
#' Internally, the data are transformed on the `log2` scale following
#' `glycowork::clr_transformation()`, then back-transformed to the original
#' ratio space before returning the result.
#'
#' @details
#' The stochastic branch follows `glycowork`: when `gamma > 0`, CLR noise is
#' sampled per feature-sample entry rather than once per sample. Without
#' informed scales, this corresponds to jittering around the sample-specific
#' `log2` geometric mean of the non-zero components and then returning to ratio
#' space.
#'
#' The `group_scales` argument is interpreted in the same way as
#' `glycowork`'s `custom_scale`. For exactly two groups, provide the total-signal
#' ratio of the second group relative to the first, either directly as a scalar
#' or implicitly through two per-group scales. For multi-group data, provide one
#' positive scale per group; these scales are used only in the stochastic branch.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (for `glyexp_experiment`
#'   input) or a factor/vector with one value per sample.
#' @param gamma Standard deviation of the scale-uncertainty model on the `log2`
#'   scale. Default is `0.1`. Set to `0` for deterministic CLR.
#' @param group_scales Optional informed group scales. For binary comparisons,
#'   this can be a single positive ratio for the second group relative to the
#'   first, or two positive scales from which that ratio is derived. For
#'   multi-group data, provide a positive vector with one scale per group.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with CLR-transformed expression matrix.
#'   If `x` is a matrix, returns a CLR-transformed matrix.
#'   The returned values are back-transformed to the original ratio space.
#'   Zeros in the input therefore remain zeros in the output.
#' @export
normalize_clr <- function(x, by = NULL, gamma = 0.1, group_scales = NULL) {
  UseMethod("normalize_clr")
}

#' @rdname normalize_clr
#' @export
normalize_clr.glyexp_experiment <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .dispatch_on_input(
    x,
    .normalize_clr_exp,
    .normalize_clr_mat,
    by = by,
    gamma = gamma,
    group_scales = group_scales
  )
}

#' @rdname normalize_clr
#' @export
normalize_clr.matrix <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .normalize_clr_mat(x, by = by, gamma = gamma, group_scales = group_scales)
}

#' @rdname normalize_clr
#' @export
normalize_clr.default <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


#' Additive Log-Ratio Normalization
#'
#' This function implements a glycoWork-compatible ALR preprocessing strategy.
#' Internally, the data are transformed relative to an automatically selected
#' reference glycan using glycoWork-style ALR logic, then back-transformed to
#' the original ratio space before returning the result.
#'
#' @details
#' Candidate reference glycans are ranked using the product of their Procrustes
#' correlation to the corresponding CLR geometry and the inverse of their
#' between-group variance, using the same fallback thresholds as `glycowork`.
#' If the best candidate has Procrustes correlation below `0.9` or
#' between-group variance above `0.1`, ALR is abandoned and CLR is returned.
#'
#' The reference search only considers glycans that are strictly positive in all
#' samples, because the reference must remain finite on the `log2` scale.
#' Non-reference glycans may still contain zeros; those entries remain zero in
#' the returned ratio-space output.
#'
#' When `gamma > 0`, successful ALR transformations also include glycoWork-style
#' uncertainty on the reference scale rather than reserving `gamma` only for
#' CLR fallback.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (for `glyexp_experiment`
#'   input) or a factor/vector with one value per sample. Used for reference
#'   ranking and group-aware ALR/CLR behavior.
#' @param gamma Standard deviation of the glycoWork-style uncertainty model.
#'   Default is `0.1`.
#' @param group_scales Optional informed group scales passed through to the
#'   glycoWork-style ALR/CLR logic. See [normalize_clr()].
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with an ALR-transformed expression matrix.
#'   If `x` is a matrix, returns an ALR-transformed matrix.
#'   When ALR succeeds, the reference glycan is excluded from the result and the
#'   output therefore has one fewer row than the input. When ALR falls back to
#'   CLR, the returned object keeps the original dimensions. The returned values
#'   are back-transformed to the original ratio space, corresponding to
#'   `x / x_ref`.
#' @export
normalize_alr <- function(x, by = NULL, gamma = 0.1, group_scales = NULL) {
  UseMethod("normalize_alr")
}

#' @rdname normalize_alr
#' @export
normalize_alr.glyexp_experiment <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .dispatch_on_input(
    x,
    .normalize_alr_exp,
    .normalize_alr_mat,
    by = by,
    gamma = gamma,
    group_scales = group_scales
  )
}

#' @rdname normalize_alr
#' @export
normalize_alr.matrix <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .normalize_alr_mat(x, by = by, gamma = gamma, group_scales = group_scales)
}

#' @rdname normalize_alr
#' @export
normalize_alr.default <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}


# ---------- Implementation ----------
.normalize_median <- function(mat) {
  normed <- limma::normalizeMedianValues(mat)
  if (any(colMeans(is.nan(normed)) == 1)) {
    cli::cli_warn(
      "Some samples have median values of {.val 0}, producing all NaNs after normalization."
    )
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
  2^normed
}


.normalize_loesscyc <- function(mat, ...) {
  log_mat <- log2(mat)
  normed <- limma::normalizeCyclicLoess(log_mat, method = "pairs", ...)
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2^normed
}


.normalize_vsn <- function(mat, ...) {
  rlang::check_installed("vsn", reason = "to use `normalize_vsn()`")
  if (nrow(mat) < 42) {
    rlang::abort(
      "The number of variables should be at least 42 for `normalize_vsn()`"
    )
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
  2^normed
}


.normalize_rlrma <- function(mat) {
  rlang::check_installed("MASS", reason = "to use `normalize_rlrma()`")
  normed <- log2(mat)
  ref_sample <- matrixStats::rowMedians(normed, na.rm = TRUE, useNames = TRUE)
  for (i in seq_len(ncol(normed))) {
    sample <- normed[, i]
    m <- sample - ref_sample # MA transformation
    lr_fit <- suppressWarnings(MASS::rlm(
      m ~ ref_sample,
      na.action = stats::na.exclude,
      maxit = 50
    ))
    fit_values <- stats::predict(lr_fit)
    normed[, i] <- m - fit_values
  }
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2^normed
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
        fit <- suppressWarnings(MASS::rlm(
          m ~ a,
          na.action = stats::na.exclude,
          maxit = 50
        ))
        fit_values <- stats::predict(fit)
        normed[, i] <- sample1 - fit_values / 2
        normed[, j] <- sample2 + fit_values / 2
      }
    }
  }
  colnames(normed) <- colnames(mat)
  rownames(normed) <- rownames(mat)
  2^normed
}


#' Validate compositional inputs used by CLR and ALR
#'
#' @param mat A matrix with variables as rows and samples as columns.
#' @param method Name of the calling method for error messages.
#'
#' @return Invisibly returns `TRUE`.
#' @keywords internal
#' @noRd
.validate_coda_input <- function(mat, method) {
  checkmate::assert_matrix(mat, mode = "numeric")

  if (any(is.na(mat))) {
    cli::cli_abort(c(
      "Missing values are not allowed for {.val {method}} transformation.",
      "x" = "Found {.val {sum(is.na(mat))}} missing value(s) in the data.",
      "i" = "Impute missing values before applying {.val {method}} transformation."
    ))
  }

  if (any(mat < 0, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Negative values are not allowed for {.val {method}} transformation.",
      "x" = "Found {.val {sum(mat < 0, na.rm = TRUE)}} negative value(s) in the data.",
      "i" = "Only zero and positive abundances are valid for compositional log-ratio transformations."
    ))
  }

  invisible(TRUE)
}

#' Resolve glycoWork-style group assignments from `by`
#'
#' @param by_values Optional grouping vector with one value per sample.
#' @param n_samples Number of samples.
#'
#' @return A list with sample indices for `group1` and `group2`, plus labels.
#' @keywords internal
#' @noRd
.resolve_coda_groups <- function(by_values, n_samples) {
  if (is.null(by_values)) {
    return(list(
      group1_idx = seq_len(n_samples),
      group2_idx = integer(),
      labels = rep(NA_character_, n_samples),
      unique_groups = character()
    ))
  }

  labels <- as.character(by_values)
  if (any(is.na(labels))) {
    cli::cli_abort("Grouping values cannot contain missing values.")
  }

  unique_groups <- unique(labels)
  if (length(unique_groups) == 2) {
    return(list(
      group1_idx = which(labels == unique_groups[1]),
      group2_idx = which(labels == unique_groups[2]),
      labels = labels,
      unique_groups = unique_groups
    ))
  }

  list(
    group1_idx = seq_len(n_samples),
    group2_idx = integer(),
    labels = labels,
    unique_groups = unique_groups
  )
}

#' Resolve `group_scales` to glycoWork-style custom scale inputs
#'
#' @param by_values Optional grouping vector with one value per sample.
#' @param group_scales Optional positive ratio(s) or per-group scales.
#'
#' @return A list describing the custom scale mode.
#' @keywords internal
#' @noRd
.resolve_custom_scale <- function(by_values, group_scales) {
  if (is.null(group_scales)) {
    return(list(type = "none", value = NULL))
  }

  checkmate::assert_numeric(group_scales, lower = 0, any.missing = FALSE)

  if (is.null(by_values)) {
    cli::cli_abort(
      "The {.arg by} parameter must be supplied when {.arg group_scales} is used."
    )
  }

  groups <- as.character(by_values)
  if (any(is.na(groups))) {
    cli::cli_abort(
      "Grouping values cannot contain missing values when {.arg group_scales} is used."
    )
  }

  unique_groups <- unique(groups)
  if (length(unique_groups) == 2) {
    if (length(group_scales) == 1) {
      return(list(type = "numeric", value = as.numeric(group_scales)))
    }

    if (is.null(names(group_scales))) {
      if (length(group_scales) != 2) {
        cli::cli_abort(
          "Unnamed {.arg group_scales} must have length 2 for two-group data."
        )
      }
      scale_map <- as.numeric(group_scales)
      names(scale_map) <- unique_groups
    } else {
      missing_groups <- setdiff(unique_groups, names(group_scales))
      if (length(missing_groups) > 0) {
        cli::cli_abort(c(
          "Every group must have a known scale.",
          "x" = "Missing scale(s) for group(s): {.val {missing_groups}}."
        ))
      }
      scale_map <- as.numeric(group_scales[unique_groups])
      names(scale_map) <- unique_groups
    }

    return(list(
      type = "numeric",
      value = unname(scale_map[unique_groups[2]] / scale_map[unique_groups[1]])
    ))
  }

  if (is.null(names(group_scales))) {
    if (length(group_scales) != length(unique_groups)) {
      cli::cli_abort(
        "Unnamed {.arg group_scales} must have the same length as the number of groups."
      )
    }
    scale_map <- as.numeric(group_scales)
    names(scale_map) <- unique_groups
  } else {
    missing_groups <- setdiff(unique_groups, names(group_scales))
    if (length(missing_groups) > 0) {
      cli::cli_abort(c(
        "Every group must have a known scale.",
        "x" = "Missing scale(s) for group(s): {.val {missing_groups}}."
      ))
    }
    scale_map <- as.numeric(group_scales[unique_groups])
    names(scale_map) <- unique_groups
  }

  list(type = "dict", value = scale_map)
}

#' Compute column-wise geometric means, omitting zeros
#'
#' @param mat A numeric matrix.
#'
#' @return A numeric vector of column geometric means.
#' @keywords internal
#' @noRd
.column_geometric_mean <- function(mat) {
  apply(mat, 2, function(values) {
    positive_values <- values[values > 0]
    if (length(positive_values) == 0) {
      return(NaN)
    }

    exp(mean(log(positive_values)))
  })
}

#' Sample a matrix of normal deviates using a matrix of means
#'
#' @param mean_mat Matrix of means.
#' @param sd Standard deviation.
#'
#' @return Numeric matrix with the same shape as `mean_mat`.
#' @keywords internal
#' @noRd
.rnorm_matrix <- function(mean_mat, sd) {
  matrix(
    stats::rnorm(length(mean_mat), mean = c(mean_mat), sd = sd),
    nrow = nrow(mean_mat),
    ncol = ncol(mean_mat)
  )
}

#' Internal glycoWork-compatible CLR on the `log2` scale
#'
#' @param mat A matrix with variables as rows and samples as columns.
#' @param by_values Optional grouping vector with one value per sample.
#' @param gamma Standard deviation of the uncertainty model.
#' @param group_scales Optional known group-level scales.
#'
#' @return A CLR-transformed matrix on the `log2` scale.
#' @keywords internal
#' @noRd
.glycowork_clr_log <- function(mat, by_values = NULL, gamma = 0.1, group_scales = NULL) {
  groups <- .resolve_coda_groups(by_values, ncol(mat))
  custom_scale <- .resolve_custom_scale(by_values, group_scales)
  geometric_mean <- .column_geometric_mean(mat)
  log_mat <- log2(mat)
  clr_adjusted <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))

  if (gamma > 0 && custom_scale$type != "dict") {
    group1_idx <- groups$group1_idx
    group2_idx <- if (length(groups$group2_idx) > 0) groups$group2_idx else group1_idx
    geometric_mean <- -log2(geometric_mean)

    if (length(groups$group2_idx) > 0) {
      if (custom_scale$type == "numeric") {
        group1_noise <- .rnorm_matrix(
          matrix(0, nrow = nrow(mat), ncol = length(group1_idx)),
          gamma
        )
        clr_adjusted[, group1_idx] <- log_mat[, group1_idx, drop = FALSE] + group1_noise

        group2_means <- matrix(
          log2(custom_scale$value),
          nrow = nrow(mat),
          ncol = length(group2_idx)
        )
        group2_noise <- .rnorm_matrix(group2_means, gamma)
      } else {
        clr_adjusted[, group1_idx] <- sweep(
          log_mat[, group1_idx, drop = FALSE],
          2,
          geometric_mean[group1_idx],
          "+"
        )

        group2_means <- matrix(
          geometric_mean[group2_idx],
          nrow = nrow(mat),
          ncol = length(group2_idx),
          byrow = TRUE
        )
        group2_noise <- .rnorm_matrix(group2_means, gamma)
      }

      clr_adjusted[, group2_idx] <- log_mat[, group2_idx, drop = FALSE] + group2_noise
    } else {
      group_means <- matrix(
        geometric_mean[group1_idx],
        nrow = nrow(mat),
        ncol = length(group1_idx),
        byrow = TRUE
      )
      clr_adjusted[, group1_idx] <- log_mat[, group1_idx, drop = FALSE] +
        .rnorm_matrix(group_means, gamma)
    }
  } else if (custom_scale$type == "dict" && length(groups$group2_idx) == 0) {
    gamma <- max(gamma, 0.1)
    for (idx in seq_len(ncol(mat))) {
      scale_factor <- custom_scale$value[[groups$labels[idx]]]
      clr_adjusted[, idx] <- log_mat[, idx] + stats::rnorm(
        nrow(mat),
        mean = log2(scale_factor),
        sd = gamma
      )
    }
  } else {
    clr_adjusted <- sweep(log_mat, 2, log2(geometric_mean), "-")
  }

  colnames(clr_adjusted) <- colnames(mat)
  rownames(clr_adjusted) <- rownames(mat)
  clr_adjusted
}

#' Center a matrix with the paper-specific CLR transform
#'
#' @param mat A matrix with variables as rows and samples as columns.
#' @param by Optional grouping vector with one value per sample.
#' @param gamma Standard deviation of the scale-uncertainty model.
#' @param group_scales Optional known group-level scales.
#'
#' @return A CLR-transformed matrix in the original ratio space.
#' @keywords internal
#' @noRd
.normalize_clr_mat <- function(
  mat,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .validate_coda_input(mat, "CLR")
  checkmate::assert_number(gamma, lower = 0, finite = TRUE)

  by_values <- .resolve_column_param(
    by,
    sample_info = NULL,
    param_name = "by",
    n_samples = ncol(mat),
    allow_null = TRUE
  )
  result <- 2^.glycowork_clr_log(
    mat,
    by_values = by_values,
    gamma = gamma,
    group_scales = group_scales
  )
  colnames(result) <- colnames(mat)
  rownames(result) <- rownames(mat)
  result
}

#' Apply paper-specific CLR to an experiment object
#'
#' @param exp A `glyexp_experiment`.
#' @param by Grouping specification.
#' @param gamma Standard deviation of the scale-uncertainty model.
#' @param group_scales Optional known group-level scales.
#'
#' @return A modified `glyexp_experiment`.
#' @keywords internal
#' @noRd
.normalize_clr_exp <- function(
  exp,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  by_values <- .resolve_column_param(
    by,
    sample_info = exp$sample_info,
    param_name = "by",
    n_samples = ncol(exp$expr_mat),
    allow_null = TRUE
  )

  exp$expr_mat <- .normalize_clr_mat(
    exp$expr_mat,
    by = by_values,
    gamma = gamma,
    group_scales = group_scales
  )
  exp$meta_data$coda_transform <- "clr"
  exp$meta_data$coda_reference <- NULL
  exp
}

#' Compute between-group variance for a candidate ALR reference
#'
#' @param values A positive numeric vector with one value per sample.
#' @param by_values Optional grouping vector with one value per sample.
#'
#' @return A non-negative scalar variance.
#' @keywords internal
#' @noRd
.between_group_variance <- function(values, by_values = NULL) {
  log_values <- log2(values)
  groups <- .resolve_coda_groups(by_values, length(values))

  if (length(groups$group2_idx) > 0) {
    var_group1 <- stats::var(log_values[groups$group1_idx])
    var_group2 <- stats::var(log_values[groups$group2_idx])
    return(abs(var_group1 - var_group2))
  }

  abs(stats::var(log_values[groups$group1_idx]))
}

#' Perform ALR with a fixed reference glycan
#'
#' @param mat A matrix with variables as rows and samples as columns.
#' @param ref_idx Integer index of the reference glycan.
#'
#' @return An ALR-transformed matrix without the reference row, in the original
#'   ratio space.
#' @keywords internal
#' @noRd
.apply_alr_reference <- function(
  mat,
  ref_idx,
  by_values = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  groups <- .resolve_coda_groups(by_values, ncol(mat))
  custom_scale <- .resolve_custom_scale(by_values, group_scales)
  log_mat <- log2(mat)
  ref_log <- log_mat[ref_idx, , drop = TRUE]
  result <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  group1_idx <- groups$group1_idx
  group2_idx <- if (length(groups$group2_idx) > 0) groups$group2_idx else group1_idx

  if (custom_scale$type != "dict") {
    if (custom_scale$type == "numeric") {
      group1_noise <- stats::rnorm(length(group1_idx), mean = log2(1), sd = gamma)
      result[, group1_idx] <- sweep(
        log_mat[, group1_idx, drop = FALSE],
        2,
        ref_log[group1_idx] - group1_noise,
        "-"
      )
    } else {
      result[, group1_idx] <- sweep(
        log_mat[, group1_idx, drop = FALSE],
        2,
        ref_log[group1_idx],
        "-"
      )
    }

    scale_adjustment <- if (custom_scale$type == "numeric") {
      log2(custom_scale$value)
    } else {
      0
    }
    group2_noise <- stats::rnorm(length(group2_idx), mean = scale_adjustment, sd = gamma)
    result[, group2_idx] <- sweep(
      log_mat[, group2_idx, drop = FALSE],
      2,
      ref_log[group2_idx] - group2_noise,
      "-"
    )
  } else {
    gamma <- max(gamma, 0.1)
    for (idx in seq_len(ncol(mat))) {
      scale_factor <- custom_scale$value[[groups$labels[idx]]]
      reference_adjusted <- ref_log[idx] - stats::rnorm(
        1,
        mean = log2(scale_factor),
        sd = gamma
      )
      result[, idx] <- log_mat[, idx] - reference_adjusted
    }
  }

  result <- 2^result
  keep_idx <- setdiff(seq_len(nrow(mat)), ref_idx)
  result <- result[keep_idx, , drop = FALSE]
  colnames(result) <- colnames(mat)
  rownames(result) <- rownames(mat)[keep_idx]
  result
}

#' Reduce a sample configuration to a comparable rank
#'
#' @param config A numeric matrix with samples as rows.
#' @param k Number of dimensions to keep.
#'
#' @return A reduced configuration matrix.
#' @keywords internal
#' @noRd
.reduce_configuration <- function(config, k) {
  centered <- scale(config, center = TRUE, scale = FALSE)
  svd_fit <- svd(centered)
  k <- min(k, length(svd_fit$d))

  if (k < 1) {
    return(matrix(0, nrow = nrow(config), ncol = 1))
  }

  svd_fit$u[, seq_len(k), drop = FALSE] %*%
    diag(svd_fit$d[seq_len(k)], nrow = k)
}

#' Compute Procrustes correlation between CLR and ALR sample geometries
#'
#' @param clr_mat A CLR-transformed matrix with variables as rows.
#' @param alr_mat An ALR-transformed matrix with variables as rows.
#'
#' @return A scalar Procrustes correlation in `[0, 1]`.
#' @keywords internal
#' @noRd
.procrustes_correlation <- function(clr_mat, alr_mat) {
  clr_config <- t(clr_mat)
  alr_config <- t(alr_mat)

  k <- min(
    qr(scale(clr_config, center = TRUE, scale = FALSE))$rank,
    qr(scale(alr_config, center = TRUE, scale = FALSE))$rank
  )

  if (k < 1) {
    return(0)
  }

  clr_reduced <- .reduce_configuration(clr_config, k)
  alr_reduced <- .reduce_configuration(alr_config, k)

  clr_scaled <- clr_reduced / sqrt(sum(clr_reduced^2))
  alr_scaled <- alr_reduced / sqrt(sum(alr_reduced^2))

  sum(svd(t(clr_scaled) %*% alr_scaled)$d)
}

#' Select the best ALR reference glycan from fully positive glycans
#'
#' @param mat A matrix with variables as rows and samples as columns.
#' @param by_values Optional grouping vector with one value per sample.
#'
#' @return A list describing the best reference candidate.
#' @keywords internal
#' @noRd
.select_alr_reference <- function(mat, by_values = NULL) {
  candidate_idx <- which(rowSums(mat <= 0, na.rm = TRUE) == 0)

  if (length(candidate_idx) < 2) {
    return(NULL)
  }

  scoring_mat <- mat[candidate_idx, , drop = FALSE]
  clr_mat <- .glycowork_clr_log(scoring_mat, by_values = by_values, gamma = 0.01)

  stats_list <- purrr::map(
    seq_len(nrow(scoring_mat)),
    function(idx) {
      alr_mat <- log2(.apply_alr_reference(
        scoring_mat,
        idx,
        by_values = by_values,
        gamma = 0.01
      ))
      proc_corr <- .procrustes_correlation(clr_mat, alr_mat)
      ref_var <- .between_group_variance(scoring_mat[idx, ], by_values)

      list(
        ref_idx = candidate_idx[idx],
        reference = rownames(mat)[candidate_idx[idx]],
        procrustes = proc_corr,
        variance = ref_var,
        score = proc_corr / ref_var
      )
    }
  )

  scores <- purrr::map_dbl(stats_list, "score")
  stats_list[[which.max(scores)]]
}

#' Apply paper-specific ALR with CLR fallback
#'
#' @param mat A matrix with variables as rows and samples as columns.
#' @param by Optional grouping vector with one value per sample.
#' @param gamma Standard deviation of the CLR uncertainty model used in fallback.
#' @param group_scales Optional known group-level scales used in fallback.
#'
#' @return A matrix transformed by ALR or CLR fallback.
#' @keywords internal
#' @noRd
.normalize_alr_mat <- function(
  mat,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .validate_coda_input(mat, "ALR")
  checkmate::assert_number(gamma, lower = 0, finite = TRUE)

  by_values <- .resolve_column_param(
    by,
    sample_info = NULL,
    param_name = "by",
    n_samples = ncol(mat),
    allow_null = TRUE
  )

  ref_stats <- .select_alr_reference(mat, by_values = by_values)
  if (is.null(ref_stats)) {
    cli::cli_warn(
      "ALR requires at least two glycans with positive values in every sample; falling back to CLR."
    )
    return(.normalize_clr_mat(
      mat,
      by = by_values,
      gamma = gamma,
      group_scales = group_scales
    ))
  }

  if (ref_stats$procrustes < 0.9 || ref_stats$variance > 0.1) {
    cli::cli_warn(c(
      "Best ALR reference {.val {ref_stats$reference}} did not meet the paper thresholds; falling back to CLR.",
      "i" = "Procrustes correlation = {.val {signif(ref_stats$procrustes, 4)}}, between-group variance = {.val {signif(ref_stats$variance, 4)}}."
    ))
    return(.normalize_clr_mat(
      mat,
      by = by_values,
      gamma = gamma,
      group_scales = group_scales
    ))
  }

  .apply_alr_reference(
    mat,
    ref_stats$ref_idx,
    by_values = by_values,
    gamma = gamma,
    group_scales = group_scales
  )
}

#' Apply paper-specific ALR to an experiment object
#'
#' @param exp A `glyexp_experiment`.
#' @param by Grouping specification.
#' @param gamma Standard deviation of the CLR uncertainty model used in fallback.
#' @param group_scales Optional known group-level scales used in fallback.
#'
#' @return A modified `glyexp_experiment`.
#' @keywords internal
#' @noRd
.normalize_alr_exp <- function(
  exp,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  by_values <- .resolve_column_param(
    by,
    sample_info = exp$sample_info,
    param_name = "by",
    n_samples = ncol(exp$expr_mat),
    allow_null = TRUE
  )

  original_rows <- rownames(exp$expr_mat)
  transformed <- .normalize_alr_mat(
    exp$expr_mat,
    by = by_values,
    gamma = gamma,
    group_scales = group_scales
  )

  if (nrow(transformed) < nrow(exp$expr_mat)) {
    keep_idx <- original_rows %in% rownames(transformed)
    exp$var_info <- exp$var_info[keep_idx, , drop = FALSE]
    exp$meta_data$coda_transform <- "alr"
    exp$meta_data$coda_reference <- setdiff(
      original_rows,
      rownames(transformed)
    )
  } else {
    exp$meta_data$coda_transform <- "clr"
    exp$meta_data$coda_reference <- NULL
  }

  exp$expr_mat <- transformed
  exp
}
