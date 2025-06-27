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

.update_expr_mat <- function(exp, matrix_func, by, ...) {
  # Validate `by` argument
  if (!is.null(by)) {
    checkmate::check_string(by)
    if (by == "sample") {
      cli::cli_abort("Can't stratify by {.val {by}}.")
    }
    if (!by %in% colnames(exp$sample_info)) {
      cli::cli_abort("The column {.val {by}} does not exist in {.var sample_info}.")
    }
  }

  # Perform processing
  if (is.null(by)) {
    new_expr_mat <- matrix_func(exp$expr_mat, ...)
  } else {
    samples <- colnames(exp$expr_mat)
    samples_grouped <- split(samples, exp$sample_info[[by]])
    mats_grouped <- purrr::map(samples_grouped, ~ exp$expr_mat[, .x])
    new_mats_grouped <- purrr::map(mats_grouped, matrix_func, ...)
    new_expr_mat <- do.call(cbind, new_mats_grouped)[, samples]
  }

  exp$expr_mat <- new_expr_mat
  exp
}
