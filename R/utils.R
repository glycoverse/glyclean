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

#' Check whether an object is a supported glyclean container
#'
#' @param x An object.
#'
#' @return A logical scalar.
#' @noRd
.is_glyclean_container <- function(x) {
  inherits(x, "glyexp_experiment") ||
    glyexp::is_glycomic_se(x) ||
    glyexp::is_glycoproteomic_se(x)
}

#' Validate a glyclean container
#'
#' @param x An object.
#'
#' @return `x`, invisibly.
#' @noRd
.assert_glyclean_container <- function(x) {
  if (!.is_glyclean_container(x)) {
    cli::cli_abort(
      "Must inherit from class 'glyexp_experiment', 'GlycomicSE', or 'GlycoproteomicSE'."
    )
  }
  invisible(x)
}

#' Extract the abundance matrix from a glyclean container
#'
#' @param x A supported glyclean container.
#'
#' @return A matrix.
#' @noRd
.get_expr_mat <- function(x) {
  .assert_glyclean_container(x)
  if (inherits(x, "glyexp_experiment")) {
    return(glyexp::get_expr_mat(x))
  }
  SummarizedExperiment::assay(x)
}

#' Extract sample information from a glyclean container
#'
#' @param x A supported glyclean container.
#'
#' @return A tibble with a `sample` identifier column.
#' @noRd
.get_sample_info <- function(x) {
  .assert_glyclean_container(x)
  if (inherits(x, "glyexp_experiment")) {
    return(glyexp::get_sample_info(x))
  }
  tibble::as_tibble(
    SummarizedExperiment::colData(x),
    rownames = "sample"
  )
}

#' Extract variable information from a glyclean container
#'
#' @param x A supported glyclean container.
#'
#' @return A tibble with a `variable` identifier column.
#' @noRd
.get_var_info <- function(x) {
  .assert_glyclean_container(x)
  if (inherits(x, "glyexp_experiment")) {
    return(glyexp::get_var_info(x))
  }
  tibble::as_tibble(
    SummarizedExperiment::rowData(x),
    rownames = "variable"
  )
}

#' Extract the experiment type from a glyclean container
#'
#' @param x A supported glyclean container.
#'
#' @return The experiment type.
#' @noRd
.get_exp_type <- function(x) {
  .assert_glyclean_container(x)
  if (glyexp::is_glycomic_se(x)) {
    return("glycomics")
  }
  if (glyexp::is_glycoproteomic_se(x)) {
    return("glycoproteomics")
  }
  glyexp::get_exp_type(x)
}

#' Extract container metadata
#'
#' @param x A supported glyclean container.
#'
#' @return A metadata list.
#' @noRd
.get_container_metadata <- function(x) {
  .assert_glyclean_container(x)
  if (inherits(x, "glyexp_experiment")) {
    return(x$meta_data)
  }
  S4Vectors::metadata(x)
}

#' Rebuild a glyclean container with updated data
#'
#' @param x A supported glyclean container that supplies the output class.
#' @param expr_mat The new abundance matrix.
#' @param sample_info Sample information including a `sample` column.
#' @param var_info Variable information including a `variable` column.
#' @param metadata Container metadata.
#'
#' @return A container with the same class as `x`.
#' @noRd
.rebuild_container <- function(
  x,
  expr_mat = .get_expr_mat(x),
  sample_info = .get_sample_info(x),
  var_info = .get_var_info(x),
  metadata = .get_container_metadata(x)
) {
  .assert_glyclean_container(x)

  if (inherits(x, "glyexp_experiment")) {
    x$expr_mat <- expr_mat
    x$sample_info <- sample_info
    x$var_info <- var_info
    x$meta_data <- metadata
    return(x)
  }

  sample_info <- tibble::as_tibble(sample_info)
  var_info <- tibble::as_tibble(var_info)
  sample_names <- sample_info$sample
  variable_names <- var_info$variable
  sample_info$sample <- NULL
  var_info$variable <- NULL

  col_data <- S4Vectors::DataFrame(sample_info, row.names = sample_names)
  row_data <- S4Vectors::DataFrame(var_info, row.names = variable_names)
  assay_name <- SummarizedExperiment::assayNames(x)[[1]]
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = stats::setNames(list(expr_mat), assay_name),
    rowData = row_data,
    colData = col_data,
    metadata = metadata
  )

  if (glyexp::is_glycomic_se(x)) {
    glyexp::as_glycomic_se(se)
  } else {
    glyexp::as_glycoproteomic_se(se)
  }
}

#' Subset a glyclean container
#'
#' @param x A supported glyclean container.
#' @param rows Variable indices or names.
#' @param cols Sample indices or names.
#'
#' @return A container with the same class as `x`.
#' @noRd
.subset_container <- function(x, rows = NULL, cols = NULL) {
  mat <- .get_expr_mat(x)
  if (is.null(rows)) {
    rows <- seq_len(nrow(mat))
  }
  if (is.null(cols)) {
    cols <- seq_len(ncol(mat))
  }

  new_mat <- mat[rows, cols, drop = FALSE]
  sample_info <- .get_sample_info(x)
  var_info <- .get_var_info(x)
  sample_info <- sample_info[
    match(colnames(new_mat), sample_info$sample),
    ,
    drop = FALSE
  ]
  var_info <- var_info[
    match(rownames(new_mat), var_info$variable),
    ,
    drop = FALSE
  ]
  .rebuild_container(
    x,
    expr_mat = new_mat,
    sample_info = sample_info,
    var_info = var_info
  )
}

#' Standardize variables while preserving the input container class
#'
#' `glyexp::standardize_variable()` currently operates on legacy experiments,
#' so SE inputs cross that compatibility bridge only for this optional step.
#'
#' @param x A supported glyclean container.
#'
#' @return A container with the same class as `x`.
#' @noRd
.standardize_container_variable <- function(x) {
  if (inherits(x, "glyexp_experiment")) {
    return(glyexp::standardize_variable(x))
  }

  metadata <- .get_container_metadata(x)
  standardized <- glyexp::standardize_variable(
    glyexp::from_se(
      x,
      exp_type = .get_exp_type(x),
      glycan_type = metadata$glycan_type
    )
  )
  if (glyexp::is_glycomic_se(x)) {
    glyexp::as_glycomic_se(standardized)
  } else {
    glyexp::as_glycoproteomic_se(standardized)
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
  .assert_glyclean_container(exp)

  expr_mat <- .get_expr_mat(exp)
  sample_info <- .get_sample_info(exp)

  # Resolve by parameter
  by_values <- .resolve_column_param(
    by,
    sample_info = sample_info,
    param_name = "by",
    n_samples = ncol(expr_mat),
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
  new_expr_mat <- .apply_by_groups(expr_mat, matrix_func, by_values, ...)
  .rebuild_container(exp, expr_mat = new_expr_mat)
}
