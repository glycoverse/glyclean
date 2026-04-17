#' Centered Log-Ratio Transformation
#'
#' This function implements a glycoWork-compatible CLR preprocessing strategy.
#' Internally, the data are transformed on the `log2` scale following
#' `glycowork::clr_transformation()`, then back-transformed to the original
#' ratio space before returning the result.
#'
#' @details
#' # Algorithmic details
#'
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
#' # Motif quantification
#'
#' If you intend to use ALR/CLR transformation with motif quantification,
#' you should apply the transformation after motif quantification rather than before.
#' In another words, the recommended workflow is:
#'
#' 1. Perform data preprocessing.
#' ```r
#' clean_exp <- auto_clean(exp)
#' ```
#'
#' 2. Perform motif quantification on the cleaned experiment.
#' ```r
#' motif_exp <- glydet::quantify_motifs(clean_exp, motifs)
#' ```
#'
#' 3. Perform ALR/CLR transformation on the motif-quantified experiment.
#' ```r
#' coda_motif_exp <- auto_coda(motif_exp, by = "group", gamma = 0.1)
#' ```
#'
#' 4. Proceed with downstream analyses on the transformed motif experiment.
#' ```r
#' dea_res <- glystats::gly_ttest(coda_motif_exp)
#' ```
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param by Either a column name in `sample_info` (for `glyexp_experiment`
#'   input) or a factor/vector with one value per sample.
#' @param gamma Standard deviation of the scale-uncertainty model on the `log2`
#'   scale. Default is `0.1`. Set to `0` for deterministic transformation.
#' @param group_scales Optional informed group scales. For binary comparisons,
#'   this can be a single positive ratio for the second group relative to the
#'   first, or two positive scales from which that ratio is derived. For
#'   multi-group data, provide a positive vector with one scale per group.
#'
#' @return Returns the same type as the input. If `x` is a `glyexp_experiment`,
#'   returns a `glyexp_experiment` with transformed expression matrix.
#'   If `x` is a matrix, returns a transformed matrix.
#'   The returned values are back-transformed to the original ratio space.
#'   Zeros in the input therefore remain zeros in the output.
#' @export
transform_clr <- function(x, by = NULL, gamma = 0.1, group_scales = NULL) {
  UseMethod("transform_clr")
}

#' @rdname transform_clr
#' @export
transform_clr.glyexp_experiment <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .dispatch_on_input(
    x,
    .transform_clr_exp,
    .transform_clr_mat,
    by = by,
    gamma = gamma,
    group_scales = group_scales
  )
}

#' @rdname transform_clr
#' @export
transform_clr.matrix <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .transform_clr_mat(x, by = by, gamma = gamma, group_scales = group_scales)
}

#' @rdname transform_clr
#' @export
transform_clr.default <- function(
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


#' Additive Log-Ratio Transformation
#'
#' This function implements a glycoWork-compatible ALR preprocessing strategy.
#' Internally, the data are transformed relative to an automatically selected
#' reference glycan using glycoWork-style ALR logic, then back-transformed to
#' the original ratio space before returning the result.
#'
#' @inheritSection transform_clr Algorithmic details
#' @inheritSection transform_clr Motif quantification
#'
#' @inheritParams transform_clr
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
transform_alr <- function(x, by = NULL, gamma = 0.1, group_scales = NULL) {
  UseMethod("transform_alr")
}

#' @rdname transform_alr
#' @export
transform_alr.glyexp_experiment <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .dispatch_on_input(
    x,
    .transform_alr_exp,
    .transform_alr_mat,
    by = by,
    gamma = gamma,
    group_scales = group_scales
  )
}

#' @rdname transform_alr
#' @export
transform_alr.matrix <- function(
  x,
  by = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  .transform_alr_mat(x, by = by, gamma = gamma, group_scales = group_scales)
}

#' @rdname transform_alr
#' @export
transform_alr.default <- function(
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
.glycowork_clr_log <- function(
  mat,
  by_values = NULL,
  gamma = 0.1,
  group_scales = NULL
) {
  groups <- .resolve_coda_groups(by_values, ncol(mat))
  custom_scale <- .resolve_custom_scale(by_values, group_scales)
  geometric_mean <- .column_geometric_mean(mat)
  log_mat <- log2(mat)
  clr_adjusted <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))

  if (gamma > 0 && custom_scale$type != "dict") {
    group1_idx <- groups$group1_idx
    group2_idx <- if (length(groups$group2_idx) > 0) {
      groups$group2_idx
    } else {
      group1_idx
    }
    geometric_mean <- -log2(geometric_mean)

    if (length(groups$group2_idx) > 0) {
      if (custom_scale$type == "numeric") {
        group1_noise <- .rnorm_matrix(
          matrix(0, nrow = nrow(mat), ncol = length(group1_idx)),
          gamma
        )
        clr_adjusted[, group1_idx] <- log_mat[, group1_idx, drop = FALSE] +
          group1_noise

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

      clr_adjusted[, group2_idx] <- log_mat[, group2_idx, drop = FALSE] +
        group2_noise
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
      clr_adjusted[, idx] <- log_mat[, idx] +
        stats::rnorm(
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
.transform_clr_mat <- function(
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
.transform_clr_exp <- function(
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

  exp$expr_mat <- .transform_clr_mat(
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
  group2_idx <- if (length(groups$group2_idx) > 0) {
    groups$group2_idx
  } else {
    group1_idx
  }

  if (custom_scale$type != "dict") {
    if (custom_scale$type == "numeric") {
      group1_noise <- stats::rnorm(
        length(group1_idx),
        mean = log2(1),
        sd = gamma
      )
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
    group2_noise <- stats::rnorm(
      length(group2_idx),
      mean = scale_adjustment,
      sd = gamma
    )
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
      reference_adjusted <- ref_log[idx] -
        stats::rnorm(
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
  clr_mat <- .glycowork_clr_log(
    scoring_mat,
    by_values = by_values,
    gamma = 0.01
  )

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
.transform_alr_mat <- function(
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
    return(.transform_clr_mat(
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
    return(.transform_clr_mat(
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
.transform_alr_exp <- function(
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
  transformed <- .transform_alr_mat(
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
