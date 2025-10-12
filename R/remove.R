# ===== Remove Rare =====

#' Remove Rare Variables with Too Many Missing Values
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param prop The proportion of missing values to use as a threshold.
#' Variables with missing values above this threshold will be removed.
#' Defaults to 0.5.
#' @param n The number of missing values to use as a threshold.
#' An alternative to `prop`.
#' @param by Either a column name in `sample_info` (string) or a factor/vector
#'   specifying group assignments for each sample.
#' Missing value counts or proportions will be calculated within each group.
#' @param strict Works with `by`. If `FALSE`, remove a variable only if
#' it passes the missing threshold in all groups.
#' If `TRUE`, remove a variable if it passes the missing threshold in any group.
#' See examples for more details.
#' @param min_n The minimum number of non-missing values required for a variable
#' to be kept. If `NULL` (default), it is calculated dynamically:
#' - For datasets with 1, 2, or 3 samples: min_n equals the sample count
#' - For datasets with >3 samples: min_n = 3
#' - When using `by`, the rule is applied within each group
#'
#' @examples
#' # With glyexp_experiment
#' exp <- glyexp::toy_experiment
#' exp$expr_mat[1, 1] <- NA    # V1: 1/6 missing
#' exp$expr_mat[2, 1:3] <- NA  # V2: 3/6 missing
#' exp$expr_mat[3, 1:5] <- NA  # V3: 5/6 missing
#' exp$expr_mat[4, 1:6] <- NA  # V4: 6/6 missing
#' exp$expr_mat
#'
#' # Remove variables with more than 50% missing values.
#' remove_rare(exp, prop = 0.5)$expr_mat
#'
#' # Remove variables with more than 2 missing values.
#' remove_rare(exp, n = 2)$expr_mat
#'
#' # Remove variables if they have more than 1 missing value in all groups.
#' # In another word, keep variables as long as they have 1 or 0 missing value
#' # in any group.
#' remove_rare(exp, by = "group", strict = FALSE)$expr_mat
#'
#' # Keep only variables with no missing values.
#' remove_rare(exp, prop = 0)$expr_mat
#'
#' # Use custom min_n to require at least 4 non-missing values
#' remove_rare(exp, min_n = 4)$expr_mat
#'
#' # With matrix
#' mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3)
#' mat_filtered <- remove_rare(mat, prop = 0.5)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return For `glyexp_experiment` input, returns a modified `glyexp_experiment` object.
#'   For matrix input, returns a filtered matrix.
#' @export
remove_rare <- function(x, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL) {
  .dispatch_on_input(
    x,
    fun_exp = .filter_exp_rare,
    fun_mat = .filter_matrix_rare,
    prop = prop, n = n, by = by, strict = strict, min_n = min_n
  )
}

.filter_exp_rare <- function(x, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL) {
  # Resolve by parameter
  by_values <- .resolve_column_param(
    by,
    sample_info = x$sample_info,
    param_name = "by",
    n_samples = ncol(x$expr_mat),
    allow_null = TRUE
  )

  # Filter the matrix
  filtered_mat <- .filter_matrix_rare(
    x$expr_mat, prop, n, by_values, strict, min_n
  )

  # Apply filtering to the experiment object
  x$expr_mat <- filtered_mat
  x$var_info <- x$var_info %>%
    dplyr::filter(.data$variable %in% rownames(x$expr_mat))
  x
}

.filter_matrix_rare <- function(x, prop = NULL, n = NULL, by = NULL, strict = FALSE, min_n = NULL) {
  # For matrix input, by must be a vector (not a column name)
  if (!is.null(by) && is.character(by) && length(by) == 1) {
    cli::cli_abort("For matrix input, {.arg by} must be a vector, not a column name.")
  }

  by_values <- .resolve_column_param(
    by,
    sample_info = NULL,
    param_name = "by",
    n_samples = ncol(x),
    allow_null = TRUE
  )

  # Validate and standardize parameters
  params <- .validate_filter_params(prop, n, min_n)

  # Calculate min_n if not provided
  min_n_values <- .calculate_min_n(ncol(x), by_values, params$min_n)

  # Validate min_n against sample sizes
  .validate_min_n(ncol(x), by_values, min_n_values)

  # Filter variables based on missing values
  if (is.null(by_values)) {
    vars_to_remove <- .filter_rare_global(x, params$prop, params$n, min_n_values$global)
  } else {
    vars_to_remove <- .filter_rare_by_group(x, by_values, params$prop, params$n, min_n_values, strict)
  }

  # Apply filtering
  x[!vars_to_remove, , drop = FALSE]
}

.validate_filter_params <- function(prop, n, min_n) {
  # Set default prop if both prop and n are NULL
  if (is.null(n) && is.null(prop)) {
    prop <- 0.5
  }

  # Check mutual exclusivity
  if (!is.null(n) && !is.null(prop)) {
    rlang::abort("Only one of `prop` or `n` can be provided.")
  }

  # Validate prop
  if (!is.null(prop)) {
    checkmate::assert_number(prop, lower = 0, upper = 1)
  }

  # Validate n
  if (!is.null(n)) {
    checkmate::assert_number(n, lower = 0)
  }

  # Validate min_n
  if (!is.null(min_n)) {
    checkmate::assert_number(min_n, lower = 1)
  }

  list(prop = prop, n = n, min_n = min_n)
}

.calculate_min_n <- function(n_samples, by_values, min_n) {
  if (!is.null(min_n)) {
    # User provided min_n, use it for all
    return(list(global = min_n, by_group = NULL))
  }

  if (is.null(by_values)) {
    # Global min_n calculation
    global_min_n <- ifelse(n_samples <= 3, n_samples, 3)
    return(list(global = global_min_n, by_group = NULL))
  } else {
    # Group-wise min_n calculation
    groups <- split(seq_len(n_samples), by_values)
    group_sizes <- sapply(groups, length)
    group_min_n <- ifelse(group_sizes <= 3, group_sizes, 3)
    return(list(global = NULL, by_group = group_min_n))
  }
}

.validate_min_n <- function(n_samples, by_values, min_n_values) {
  if (is.null(by_values)) {
    # Global validation
    if (min_n_values$global > n_samples) {
      rlang::abort(paste0("min_n (", min_n_values$global, ") cannot be greater than the number of samples (", n_samples, ")."))
    }
  } else {
    # Group-wise validation
    groups <- split(seq_len(n_samples), by_values)
    group_sizes <- sapply(groups, length)

    if (is.null(min_n_values$global)) {
      # Using group-specific min_n
      for (i in seq_along(groups)) {
        if (min_n_values$by_group[i] > group_sizes[i]) {
          rlang::abort(paste0("min_n (", min_n_values$by_group[i], ") cannot be greater than the number of samples in group ", names(groups)[i], " (", group_sizes[i], ")."))
        }
      }
    } else {
      # Using global min_n for all groups
      if (any(min_n_values$global > group_sizes)) {
        problematic_groups <- names(groups)[min_n_values$global > group_sizes]
        rlang::abort(paste0("min_n (", min_n_values$global, ") cannot be greater than the number of samples in group(s): ", paste(problematic_groups, collapse = ", "), "."))
      }
    }
  }
}

.filter_rare_global <- function(expr_mat, prop, n, min_n) {
  # Calculate threshold-based removal
  if (is.null(n)) {
    vars_to_remove <- rowMeans(is.na(expr_mat)) > prop
  } else {
    vars_to_remove <- rowSums(is.na(expr_mat)) > n
  }

  # Apply min_n constraint
  non_missing_counts <- rowSums(!is.na(expr_mat))
  vars_to_remove <- vars_to_remove | (non_missing_counts < min_n)

  vars_to_remove
}

.filter_rare_by_group <- function(expr_mat, by_values, prop, n, min_n_values, strict) {
  groups <- split(seq_len(ncol(expr_mat)), by_values)

  group_results <- lapply(names(groups), function(group_name) {
    idx <- groups[[group_name]]
    group_mat <- expr_mat[, idx, drop = FALSE]

    # Calculate missing threshold result
    if (is.null(n)) {
      threshold_result <- rowMeans(is.na(group_mat)) > prop
    } else {
      threshold_result <- rowSums(is.na(group_mat)) > n
    }

    # Apply min_n constraint for this group
    non_missing_counts <- rowSums(!is.na(group_mat))
    current_min_n <- if (is.null(min_n_values$global)) {
      min_n_values$by_group[[group_name]]
    } else {
      min_n_values$global
    }
    min_n_result <- non_missing_counts < current_min_n

    # Combine threshold and min_n constraints
    threshold_result | min_n_result
  })
  names(group_results) <- names(groups)

  # Combine results based on strict parameter
  if (strict) {
    # Remove if exceeds threshold in any group
    vars_to_remove <- Reduce(`|`, group_results)
  } else {
    # Remove only if exceeds threshold in all groups
    vars_to_remove <- Reduce(`&`, group_results)
  }

  vars_to_remove
}

# ===== Remove Low Variance =====

#' Remove Variables with Low Variance
#' Remove Variables with Low Variance
#'
#' Filters variables whose variance falls below a threshold.
#' Default behavior is to remove variables with zero variance.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#' @param var_cutoff The cutoff for variance. Defaults to 0.
#' @param by A factor specifying the groupings. Defaults to NULL.
#' @param strict If `FALSE`, remove a variable only if it passes the variance threshold in all groups.
#'   If `TRUE`, remove a variable if it passes the variance threshold in any group.
#'
#' @returns For `glyexp_experiment` input, returns a modified `glyexp_experiment` object.
#'   For matrix input, returns a filtered matrix.
#'
#' @seealso [remove_low_cv()], [remove_constant()]
#' @export
remove_low_var <- function(x, var_cutoff = 0, by = NULL, strict = FALSE) {
  UseMethod("remove_low_var")
}

#' @rdname remove_low_var
#' @export
remove_low_var.glyexp_experiment <- function(x, var_cutoff = 0, by = NULL, strict = FALSE) {
  .filter_exp(x, by, strict, .filter_matrix_low_var, var_cutoff = var_cutoff)
}

#' @rdname remove_low_var
#' @export
remove_low_var.matrix <- function(x, var_cutoff = 0, by = NULL, strict = FALSE) {
  .filter_matrix_low_var(x, by = by, strict = strict, var_cutoff = var_cutoff)
}

#' @rdname remove_low_var
#' @export
remove_low_var.default <- function(x, var_cutoff = 0, by = NULL, strict = FALSE) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}

.filter_matrix_low_var <- function(x, by = NULL, strict = FALSE, var_cutoff = 0) {
  checkmate::assert_number(var_cutoff, lower = 0)
  checkmate::assert_flag(strict)

  if (is.null(by)) {
    return(.filter_matrix_low_var_global(x, var_cutoff))
  } else {
    checkmate::assert_vector(by, len = ncol(x))
    return(.filter_matrix_low_var_grouped(x, var_cutoff, by, strict))
  }
}

.filter_matrix_low_var_global <- function(x, var_cutoff) {
  # `variance` is a vector of length nrow(x)
  variance <- .summarize_vars_mat(x, .var, by = NULL)
  vars_to_remove <- variance <= var_cutoff
  x[!vars_to_remove, , drop = FALSE]
}

.filter_matrix_low_var_grouped <- function(x, var_cutoff, by_values, strict) {
  # `variance` is a matrix with nrow(x) rows and length(levels(by_values)) columns
  variance <- .summarize_vars_mat(x, .var, by = by_values)
  vars_to_remove <- apply(variance <= var_cutoff, 1, if (strict) any else all)
  x[!vars_to_remove, , drop = FALSE]
}

# ===== Remove Low CV =====

#' Remove Variables with Low Coefficient of Variation
#'
#' Filters variables whose coefficient of variation falls below a threshold.
#' Default behavior is to remove variables with zero coefficient of variation.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#' @param cv_cutoff The cutoff for coefficient of variation. Defaults to 0.
#' @param by A factor specifying the groupings. Defaults to NULL.
#' @param strict If `FALSE`, remove a variable only if it passes the coefficient of variation threshold in all groups.
#'   If `TRUE`, remove a variable if it passes the coefficient of variation threshold in any group.
#'
#' @returns For `glyexp_experiment` input, returns a modified `glyexp_experiment` object.
#'   For matrix input, returns a filtered matrix.
#'
#' @seealso [remove_low_var()]
#' @export
remove_low_cv <- function(x, cv_cutoff = 0, by = NULL, strict = FALSE) {
  UseMethod("remove_low_cv")
}

#' @rdname remove_low_cv
#' @export
remove_low_cv.glyexp_experiment <- function(x, cv_cutoff = 0, by = NULL, strict = FALSE) {
  .filter_exp(x, by, strict, .filter_matrix_low_cv, cv_cutoff = cv_cutoff)
}

#' @rdname remove_low_cv
#' @export
remove_low_cv.matrix <- function(x, cv_cutoff = 0, by = NULL, strict = FALSE) {
  .filter_matrix_low_cv(x, by = by, strict = strict, cv_cutoff = cv_cutoff)
}

#' @rdname remove_low_cv
#' @export
remove_low_cv.default <- function(x, cv_cutoff = 0, by = NULL, strict = FALSE) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}

.filter_matrix_low_cv <- function(x, by = NULL, strict = FALSE, cv_cutoff = 0) {
  checkmate::assert_number(cv_cutoff, lower = 0)
  checkmate::assert_flag(strict)

  if (is.null(by)) {
    return(.filter_matrix_low_cv_global(x, cv_cutoff))
  } else {
    checkmate::assert_vector(by, len = ncol(x))
    return(.filter_matrix_low_cv_grouped(x, cv_cutoff, by, strict))
  }
}

.filter_matrix_low_cv_global <- function(x, cv_cutoff) {
  # `cv` is a vector of length nrow(x)
  cv <- .summarize_vars_mat(x, .cv, by = NULL)
  vars_to_remove <- cv <= cv_cutoff
  x[!vars_to_remove, , drop = FALSE]
}

.filter_matrix_low_cv_grouped <- function(x, cv_cutoff, by_values, strict) {
  # `cv` is a matrix with nrow(x) rows and length(levels(by_values)) columns
  cv <- .summarize_vars_mat(x, .cv, by = by_values)
  vars_to_remove <- apply(cv <= cv_cutoff, 1, if (strict) any else all)
  x[!vars_to_remove, , drop = FALSE]
}

#' Robust CV
#'
#' The function is robust in these ways:
#' - It ignores NA values.
#' - When the mean is 0 or not finite, it returns Inf.
#' - When x is all NA, it returns Inf.
#'
#' @param x A vector of numeric values.
#' @returns A vector of CV values.
#' @noRd
.cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  if (m == 0 || !is.finite(m)) {
    return(Inf)
  } else {
    return(sd(x, na.rm = TRUE) / abs(m))
  }
}

#' Robust Variance
#'
#' The function is robust in these ways:
#' - It ignores NA values.
#' - When x is all NA, it returns Inf.
#' - When the variance is not finite, it returns Inf.
#'
#' @param x A vector of numeric values.
#' @returns A vector of variance values.
#' @noRd
.var <- function(x) {
  if (all(is.na(x))) {
    return(Inf)
  }
  res <- var(x, na.rm = TRUE)
  if (!is.finite(res)) {
    return(Inf)
  }
  res
}

# ===== Remove Constant =====
#' Remove Constant Variables
#'
#' Constant variables are variables with the same value in all samples.
#' This function is equivalent to `remove_low_var(x, var_cutoff = 0, by = by, strict = strict)`.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#' @param by Either a column name in `sample_info` (string) or a vector specifying group assignments for each sample.
#' @param strict If `FALSE`, remove a variable only if it is constant in all groups.
#'   If `TRUE`, remove a variable if it is constant in any group. Defaults to FALSE.
#'
#' @returns For `glyexp_experiment` input, returns a modified `glyexp_experiment` object.
#'   For matrix input, returns a filtered matrix.
#'
#' @seealso [remove_low_var()]
#' @export
remove_constant <- function(x, by = NULL, strict = FALSE) {
  remove_low_var(x, var_cutoff = 0, by = by, strict = strict)
}

# ===== Utilities =====

#' Summarize Values of Variables for Matrices
#'
#' @param mat A matrix with variables in rows and samples in columns.
#' @param .f A function to apply to the expression values of each variable.
#' @param by A factor specifying the groupings. Defaults to NULL.
#' @returns A vector of summary statistics for each variable, named by the rownames of the matrix.
#' @noRd
.summarize_vars_mat <- function(mat, .f, by = NULL) {
  if (is.null(by)) {
    apply(mat, 1, .f)
  } else {
    .summarize_vars_mat_by_group(mat, .f, by)
  }
}

#' Summarize Values of Variables for Matrices by Group
#'
#' @param mat A matrix with variables in rows and samples in columns.
#' @param .f A function to apply to the expression values of each variable.
#' @param by A factor specifying the groupings.
#' @returns A matrix of summary statistics for each variable with row names of `mat` as row names and levels of `by` as column names.
#' @noRd
.summarize_vars_mat_by_group <- function(mat, .f, by) {
  by_f <- forcats::fct_drop(as.factor(by))

  if (any(is.na(by_f))) {
    cli::cli_abort("{.arg by} must not contain NA for grouped summarization.")
  }

  levs <- levels(by_f)
  idx_list <- lapply(levs, function(lv) which(by_f == lv))

  cols <- lapply(idx_list, function(idx) {
    if (length(idx) == 0L) {
      rep(NA_real_, nrow(mat))
    } else {
      apply(mat[, idx, drop = FALSE], 1, .f)
    }
  })

  res <- do.call(cbind, cols)
  rownames(res) <- rownames(mat)
  colnames(res) <- levs
  res
}

#' Filter an Experiment
#'
#' @param x An `glyexp_experiment` object.
#' @param by The `by` parameter for `remove_xxx()` functions.
#' @param strict The `strict` parameter for `remove_xxx()` functions.
#' @param filter_mat_fun A function to filter the expression matrix.
#' @param ... Additional arguments to pass to `filter_mat_fun`.
#' @returns A modified `glyexp_experiment` object.
#' @noRd
.filter_exp <- function(x, by = NULL, strict = FALSE, filter_mat_fun, ...) {
  by_values <- .resolve_column_param(
    by,
    sample_info = x$sample_info,
    param_name = "by",
    n_samples = ncol(x$expr_mat),
    allow_null = TRUE
  )
  new_expr_mat <- filter_mat_fun(x$expr_mat, by_values, strict, ...)
  x$expr_mat <- new_expr_mat
  x$var_info <- x$var_info |>
    dplyr::filter(.data$variable %in% rownames(new_expr_mat))
  x
}
