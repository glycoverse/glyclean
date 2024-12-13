.update_expr_mat <- function(exp, f, by, ...) {
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
    new_expr_mat <- f(exp$expr_mat, ...)
  } else {
    samples <- colnames(exp$expr_mat)
    samples_grouped <- split(samples, exp$sample_info[[by]])
    mats_grouped <- purrr::map(samples_grouped, ~ exp$expr_mat[, .x])
    new_mats_grouped <- purrr::map(mats_grouped, f, ...)
    new_expr_mat <- do.call(cbind, new_mats_grouped)[, samples]
  }

  exp$expr_mat <- new_expr_mat
  exp
}
