# Input processing utilities

#' Resolve column specification to values
#'
#' This function handles both column names (strings) and direct factor/vector inputs.
#'
#' @param param The parameter to resolve (can be string, factor, or vector)
#' @param sample_info The sample_info data frame (for experiment objects)
#' @param param_name The name of the parameter (for error messages)
#' @param n_samples The number of samples (for validation)
#' @param allow_null Whether NULL values are allowed
#'
#' @return The resolved values as a vector, or NULL if param is NULL and allow_null is TRUE
#' @keywords internal
.resolve_column_param <- function(
  param,
  sample_info = NULL,
  param_name = "parameter",
  n_samples = NULL,
  allow_null = TRUE
) {
  # Handle NULL case
  if (is.null(param)) {
    if (allow_null) {
      return(NULL)
    } else {
      cli::cli_abort("The {.arg {param_name}} parameter cannot be NULL.")
    }
  }

  # Handle string case (column name)
  if (is.character(param) && length(param) == 1) {
    if (is.null(sample_info)) {
      cli::cli_abort(
        "Column name '{param}' provided for {.arg {param_name}}, but no sample_info available. Please provide a factor or vector instead."
      )
    }
    if (!param %in% colnames(sample_info)) {
      # When user explicitly provides a column name that doesn't exist, always error
      cli::cli_abort(
        "The column {.val {param}} does not exist in {.var sample_info}."
      )
    }
    return(sample_info[[param]])
  }

  # Handle factor/vector case
  if (is.factor(param) || is.vector(param)) {
    if (!is.null(n_samples) && length(param) != n_samples) {
      cli::cli_abort(
        "The {.arg {param_name}} vector must have length {n_samples} (number of samples), but has length {length(param)}."
      )
    }
    return(param)
  }

  # Invalid type
  cli::cli_abort(
    "The {.arg {param_name}} parameter must be either a column name (string) or a factor/vector."
  )
}

#' Apply function with optional grouping
#'
#' This function applies a matrix function with optional grouping by a factor/vector.
#'
#' @param mat The input matrix
#' @param matrix_func The function to apply to the matrix
#' @param by_values The grouping values (factor or vector), or NULL for no grouping
#' @param ... Additional arguments passed to matrix_func
#'
#' @return Processed matrix
#' @keywords internal
.apply_by_groups <- function(mat, matrix_func, by_values = NULL, ...) {
  if (is.null(by_values)) {
    # No grouping, apply function directly
    return(matrix_func(mat, ...))
  } else {
    # Apply function by groups
    samples <- colnames(mat)
    if (is.null(samples)) {
      samples <- seq_len(ncol(mat))
    }
    samples_grouped <- split(samples, by_values)
    mats_grouped <- purrr::map(samples_grouped, ~ mat[, .x, drop = FALSE])
    new_mats_grouped <- purrr::map(mats_grouped, matrix_func, ...)
    result_mat <- do.call(cbind, new_mats_grouped)[, samples]
    return(result_mat)
  }
}

#' Update an experiment expression matrix
#'
#' @param exp A `glyexp_experiment` object.
#' @param matrix_func Function to apply to the expression matrix.
#' @param by Optional grouping specification.
#' @param ... Additional arguments passed to `matrix_func`.
#'
#' @return A modified `glyexp_experiment` object.
#' @keywords internal
#' @noRd
.update_expr_mat <- function(exp, matrix_func, by = NULL, ...) {
  checkmate::assert_class(exp, "glyexp_experiment")

  # Resolve by parameter
  by_values <- .resolve_column_param(
    by,
    sample_info = exp$sample_info,
    param_name = "by",
    n_samples = ncol(exp$expr_mat),
    allow_null = TRUE
  )

  # Validate `by` argument
  if (!is.null(by_values)) {
    # Check for special case
    if (is.character(by) && length(by) == 1 && by == "sample") {
      cli::cli_abort("Can't stratify by {.val {by}}.")
    }
  }

  # Apply function with optional grouping
  exp$expr_mat <- .apply_by_groups(exp$expr_mat, matrix_func, by_values, ...)
  exp
}
