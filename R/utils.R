# Input processing utilities

#' Process input for glyclean functions
#' 
#' This function handles both glyexp_experiment objects and matrices.
#' It validates input and returns appropriate output format.
#' 
#' @param x Input data, either a glyexp_experiment object or a matrix
#' @param matrix_func Function to apply to the matrix (implementation function)
#' @param by Grouping variable for stratified processing
#' @param ... Additional arguments passed to matrix_func
#' 
#' @return Same type as input (experiment or matrix)
#' @keywords internal
.process_input <- function(x, matrix_func, by = NULL, ...) {
  # Validate input type
  if (inherits(x, "glyexp_experiment")) {
    # Process experiment object
    result_exp <- .update_expr_mat(x, matrix_func, by, ...)
    return(result_exp)
  } else if (is.matrix(x)) {
    # Process matrix directly
    if (!is.null(by)) {
      cli::cli_abort("The {.arg by} argument is only supported for {.cls glyexp_experiment} objects.")
    }
    # For matrix input, call the implementation function directly
    result_mat <- matrix_func(x, ...)
    return(result_mat)
  } else {
    cli::cli_abort("Input {.arg x} must be either a {.cls glyexp_experiment} object or a {.cls matrix}.")
  }
}

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
.resolve_column_param <- function(param, sample_info = NULL, param_name = "parameter", n_samples = NULL, allow_null = TRUE) {
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
      cli::cli_abort("Column name '{param}' provided for {.arg {param_name}}, but no sample_info available. Please provide a factor or vector instead.")
    }
    if (!param %in% colnames(sample_info)) {
      # When user explicitly provides a column name that doesn't exist, always error
      cli::cli_abort("The column {.val {param}} does not exist in {.var sample_info}.")
    }
    return(sample_info[[param]])
  }
  
  # Handle factor/vector case
  if (is.factor(param) || is.vector(param)) {
    if (!is.null(n_samples) && length(param) != n_samples) {
      cli::cli_abort("The {.arg {param_name}} vector must have length {n_samples} (number of samples), but has length {length(param)}.")
    }
    return(param)
  }
  
  # Invalid type
  cli::cli_abort("The {.arg {param_name}} parameter must be either a column name (string) or a factor/vector.")
}

.update_expr_mat <- function(exp, matrix_func, by, ...) {
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

  # Perform processing
  if (is.null(by_values)) {
    new_expr_mat <- matrix_func(exp$expr_mat, ...)
  } else {
    samples <- colnames(exp$expr_mat)
    samples_grouped <- split(samples, by_values)
    mats_grouped <- purrr::map(samples_grouped, ~ exp$expr_mat[, .x])
    new_mats_grouped <- purrr::map(mats_grouped, matrix_func, ...)
    new_expr_mat <- do.call(cbind, new_mats_grouped)[, samples]
  }

  exp$expr_mat <- new_expr_mat
  exp
}
