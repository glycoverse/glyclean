# Batch effect correction functions

#' Correct Batch Effect
#'
#' Correct batch effects in glycoproteomics/glycomics data using ComBat algorithm
#' from the sva package.
#'
#' @details
#' This function performs batch effect correction using the ComBat algorithm.
#' It requires batch information provided via the `batch` parameter.
#' If no batch information is available, the function will return the original data unchanged.
#'
#' If group information is provided via `group`,
#' the function will check for confounding between batch and group variables.
#' If batch and group are highly confounded (|Cramer's V| > `threshold`),
#' the function will issue a warning and return the original data unchanged
#' to avoid over-correction.
#'
#' When both batch and group information are available and not highly confounded,
#' the group information will be included in the model to preserve biological variation
#' while correcting for batch effects.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param batch Either a factor/character vector specifying batch assignments for each sample,
#'   or a string specifying the column name in sample_info (for experiment input only).
#'   Default to "batch" for experiment input.
#' @param group Either a factor/character vector specifying group assignments for each sample,
#'   or a string specifying the column name in sample_info (for experiment input only).
#'   If provided, it will be used as a covariate in the ComBat model.
#'   This is useful when you have an unbalanced design.
#'   Default to NULL.
#' @param check_confounding Whether to check for confounding between batch and group variables.
#'   Default to TRUE.
#' @param confounding_threshold The threshold for Cramer's V to consider batch and group variables highly confounded.
#'   Only used when `check_confounding` is TRUE. Default to 0.4.
#'
#' @return For `glyexp_experiment` input, returns a modified `glyexp_experiment` object.
#'   For matrix input, returns a batch-corrected matrix.
#'
#' @examples
#' # With glyexp_experiment and column names
#' exp <- glyexp::toy_experiment
#' exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
#' exp$sample_info$group <- c("Ctrl", "Ctrl", "Treat", "Ctrl", "Treat", "Treat")
#' corrected_exp <- correct_batch_effect(exp, batch = "batch", group = "group")
#'
#' # With matrix and factor vectors
#' mat <- matrix(abs(rnorm(200)), nrow = 20, ncol = 10)
#' batch_factor <- factor(rep(c("A", "B"), each = 5))
#' group_factor <- factor(rep(c("Ctrl", "Treat"), times = 5))
#' corrected_mat <- correct_batch_effect(mat, batch = batch_factor, group = group_factor)
#'
#' @importFrom utils capture.output
#' @export
correct_batch_effect <- function(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  method = c("combat", "limma")
) {
  rlang::check_installed("sva", reason = "to use `correct_batch_effect()`")
  method <- match.arg(method)
  UseMethod("correct_batch_effect")
}

#' @rdname correct_batch_effect
#' @export
correct_batch_effect.glyexp_experiment <- function(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  method = c("combat", "limma")
) {
  method <- match.arg(method)
  # For experiment input, extract batch and group from sample_info
  batch_group_info <- .extract_batch_group_from_experiment(x, batch, group, require_batch = TRUE)
  if (is.null(batch_group_info)) {
    cli::cli_alert_info("No batch information found in column '{batch}' of sample_info. Returning original experiment unchanged.")
    return(x)
  }

  # Apply batch correction to expression matrix
  corrected_expr_mat <- .apply_batch_correction(
    x$expr_mat,
    batch_group_info$batch,
    batch_group_info$group,
    check_confounding = check_confounding,
    confounding_threshold = confounding_threshold,
    method = method
  )

  # Return original if correction failed
  if (is.null(corrected_expr_mat)) {
    return(x)
  }

  # Update experiment with corrected expression matrix
  new_exp <- x
  new_exp$expr_mat <- corrected_expr_mat

  return(new_exp)
}

#' @rdname correct_batch_effect
#' @export
correct_batch_effect.matrix <- function(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  method = c("combat", "limma")
) {
  method <- match.arg(method)
  .correct_batch_effect_matrix(
    x, batch = batch, group = group,
    check_confounding = check_confounding,
    confounding_threshold = confounding_threshold,
    method = method
  )
}

#' @rdname correct_batch_effect
#' @export
correct_batch_effect.default <- function(
  x,
  batch = "batch",
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  method = c("combat", "limma")
) {
  method <- match.arg(method)
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}

.correct_batch_effect_matrix <- function(x, batch, group, check_confounding, confounding_threshold, method) {
  # Validate and prepare batch/group vectors
  batch_group_info <- .validate_and_prepare_batch_group(x, batch, group)
  if (is.null(batch_group_info)) {
    return(x)
  }

  # Apply batch correction
  corrected_expr_mat <- .apply_batch_correction(
    x,
    batch_group_info$batch,
    batch_group_info$group,
    check_confounding = check_confounding,
    confounding_threshold = confounding_threshold,
    method = method
  )

  # Return corrected matrix or original if correction failed
  if (is.null(corrected_expr_mat)) x else corrected_expr_mat
}


#' Detect batch effect
#'
#' Use ANOVA to detect if batch effect is present in the data.
#' If `group` is provided, it will be used as a covariate in the ANOVA model.
#'
#' @param x Either a `glyexp_experiment` object or a matrix.
#'   If a matrix, rows should be variables and columns should be samples.
#' @param batch Either a factor/character vector specifying batch assignments for each sample,
#'   or a string specifying the column name in sample_info (for experiment input only).
#'   Default to "batch" for experiment input.
#' @param group Either a factor/character vector specifying group assignments for each sample,
#'   or a string specifying the column name in sample_info (for experiment input only).
#'   If provided, it will be used as a covariate in the ANOVA model.
#'   This is useful when you have an unbalanced design.
#'   Default to NULL.
#'
#' @returns A double vector of p-values for each variable,
#'  i.e., the same length as `nrow(x)` (for matrix) or `nrow(get_expr_mat(x))` (for experiment)
#'
#' @examples
#' # With glyexp_experiment and column names
#' exp <- glyexp::toy_experiment
#' exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
#' exp$sample_info$group <- c("Ctrl", "Ctrl", "Treat", "Ctrl", "Treat", "Treat")
#' p_values <- detect_batch_effect(exp, batch = "batch", group = "group")
#'
#' # With matrix and factor vectors
#' mat <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' batch_factor <- factor(rep(c("A", "B"), each = 5))
#' group_factor <- factor(rep(c("Ctrl", "Treat"), times = 5))
#' p_values <- detect_batch_effect(mat, batch = batch_factor, group = group_factor)
#'
#' @export
detect_batch_effect <- function(x, batch = "batch", group = NULL) {
  UseMethod("detect_batch_effect")
}

#' @rdname detect_batch_effect
#' @export
detect_batch_effect.glyexp_experiment <- function(x, batch = "batch", group = NULL) {
  # For experiment input, extract batch and group from sample_info
  batch_group_info <- .extract_batch_group_from_experiment(x, batch, group, require_batch = TRUE)
  if (is.null(batch_group_info)) {
    return(rep(1, nrow(x$expr_mat)))
  }

  # Perform batch effect detection
  return(.perform_batch_effect_detection(
    x$expr_mat,
    batch_group_info$batch,
    batch_group_info$group
  ))
}

#' @rdname detect_batch_effect
#' @export
detect_batch_effect.matrix <- function(x, batch = "batch", group = NULL) {
  # Validate and prepare batch/group vectors
  batch_group_info <- .validate_and_prepare_batch_group(x, batch, group, require_batch = TRUE)
  if (is.null(batch_group_info)) {
    return(rep(1, nrow(x)))
  }

  # Perform batch effect detection
  return(.perform_batch_effect_detection(
    x,
    batch_group_info$batch,
    batch_group_info$group
  ))
}

#' @rdname detect_batch_effect
#' @export
detect_batch_effect.default <- function(x, batch = "batch", group = NULL) {
  cli::cli_abort(c(
    "{.arg x} must be a {.cls glyexp_experiment} object or a {.cls matrix}.",
    "x" = "Got {.cls {class(x)}}."
  ))
}

# Helper functions for common business logic

.extract_batch_group_from_experiment <- function(x, batch, group, require_batch = FALSE) {
  # Validate experiment input
  checkmate::assert_class(x, "glyexp_experiment")

  # Handle batch parameter with special logic for non-required cases
  batch_values <- NULL
  if (is.character(batch) && length(batch) == 1) {
    # For string input, check if column exists first when not required
    if (batch %in% colnames(x$sample_info)) {
      batch_values <- .resolve_column_param(
        batch, 
        sample_info = x$sample_info, 
        param_name = "batch", 
        n_samples = ncol(x$expr_mat),
        allow_null = FALSE
      )
    } else if (require_batch) {
      # If batch is required but column doesn't exist, let resolve_column_param handle the error
      batch_values <- .resolve_column_param(
        batch, 
        sample_info = x$sample_info, 
        param_name = "batch", 
        n_samples = ncol(x$expr_mat),
        allow_null = FALSE
      )
    } else {
      # Batch column doesn't exist and is not required
      return(NULL)
    }
  } else if (!is.null(batch)) {
    # Direct factor/vector input
    batch_values <- .resolve_column_param(
      batch, 
      sample_info = x$sample_info, 
      param_name = "batch", 
      n_samples = ncol(x$expr_mat),
      allow_null = FALSE
    )
  } else if (require_batch) {
    cli::cli_abort("Batch information is required but not provided.")
  } else {
    return(NULL)
  }

  # Resolve group parameter
  group_values <- .resolve_column_param(
    group, 
    sample_info = x$sample_info, 
    param_name = "group", 
    n_samples = ncol(x$expr_mat),
    allow_null = TRUE
  )

  # Check if batch was found
  if (is.null(batch_values) && require_batch) {
    cli::cli_abort("Batch information is required but not provided.")
  }

  if (is.null(batch_values)) {
    return(NULL)
  }

  return(list(batch = batch_values, group = group_values))
}

.validate_and_prepare_batch_group <- function(x, batch, group = NULL, require_batch = FALSE) {
  # Validate matrix input
  checkmate::assert_matrix(x)

  # Check for invalid string inputs for matrix
  if (is.character(batch) && length(batch) == 1) {
    cli::cli_abort("Column name '{batch}' provided for {.arg batch}, but no sample_info available for matrix input. Please provide a factor or vector instead.")
  }
  if (is.character(group) && length(group) == 1) {
    cli::cli_abort("Column name '{group}' provided for {.arg group}, but no sample_info available for matrix input. Please provide a factor or vector instead.")
  }

  checkmate::assert_vector(batch, len = ncol(x))
  if (!is.null(group)) {
    checkmate::assert_vector(group, len = ncol(x))
  }

  # Convert to factors if needed
  batch <- factor(batch)
  if (!is.null(group)) {
    group <- factor(group)
  }

  # Check if there are at least 2 batches
  if (length(unique(batch)) < 2) {
    if (require_batch) {
      cli::cli_warn("Less than 2 batches found. Cannot perform batch effect detection.")
    }
    return(NULL)
  }

  return(list(batch = batch, group = group))
}

.check_batch_group_confounding <- function(batch, group, threshold = 0.4) {
  confusion_table <- table(batch, group)
  abs(.cramers_v(confusion_table)) > threshold
}

.cramers_v <- function(conf_table) {
  statistic <- as.numeric(suppressWarnings(stats::chisq.test(conf_table))$statistic)
  sqrt((statistic / sum(conf_table)) / min(dim(conf_table) - 1))
}

.has_enough_samples_per_batch <- function(batch) {
  batch_counts <- table(batch)
  all(batch_counts >= 2)
}

.apply_batch_correction <- function(expr_mat, batch, group = NULL, check_confounding = TRUE, confounding_threshold = 0.4) {
  # Check if there are at least 2 batches
  if (length(unique(batch)) < 2) {
    cli::cli_warn("Less than 2 batches found. Batch correction requires at least 2 batches. Returning original data unchanged.")
    return(expr_mat)
  }

  # Check for confounding
  if (check_confounding && !is.null(group) && .check_batch_group_confounding(batch, group, confounding_threshold)) {
    cli::cli_warn(c(
      "Batch and group variables are highly confounded.",
      "i" = "Batch effect correction may not be appropriate.",
      "i" = "Returning original data unchanged."
    ))
    return(expr_mat)
  }

  # Check sufficient samples per batch
  if (!.has_enough_samples_per_batch(batch)) {
    cli::cli_warn(c(
      "Some batches have fewer than 2 samples.",
      "i" = "ComBat requires at least 2 samples per batch.",
      "i" = "Returning original data unchanged."
    ))
    return(expr_mat)
  }

  # Perform batch correction using ComBat
  log_expr_mat <- log2(expr_mat + 1)

  # Create model matrix
  if (!is.null(group)) {
    mod <- stats::model.matrix(~ group)
  } else {
    mod <- NULL
  }

  # Apply ComBat correction with error handling and suppressed output
  corrected_log_expr_mat <- tryCatch({
    # Suppress ComBat's verbose output completely
    withr::with_output_sink(
      nullfile(),
      sva::ComBat(
        dat = log_expr_mat,
        batch = batch,
        mod = mod,
        par.prior = TRUE,
        prior.plots = FALSE
      )
    )
  }, error = function(e) {
    cli::cli_warn(c(
      "ComBat failed to correct batch effects.",
      "i" = "Error: {e$message}",
      "i" = "Returning original data unchanged."
    ))
    return(expr_mat)
  })

  # Check if ComBat succeeded
  if (is.null(corrected_log_expr_mat)) {
    return(expr_mat)
  }

  # Convert back from log space
  2^corrected_log_expr_mat - 1
}

.perform_batch_effect_detection <- function(expr_mat, batch, group = NULL) {
  # Check if there are at least 2 batches
  if (length(unique(batch)) < 2) {
    cli::cli_warn("Less than 2 batches found. Cannot perform batch effect detection.")
    return(rep(1, nrow(expr_mat)))
  }

  # Prepare data for ANOVA
  n_variables <- nrow(expr_mat)

  # Function to perform ANOVA for a single variable
  perform_anova <- function(variable_values) {
    # Create data frame for ANOVA
    df <- data.frame(
      value = as.numeric(variable_values),
      batch = factor(batch)
    )

    # Add group column if provided
    if (!is.null(group)) {
      df$group <- factor(group)
    }

    # Build formula
    if (!is.null(group)) {
      formula <- stats::as.formula("value ~ batch + group")
    } else {
      formula <- stats::as.formula("value ~ batch")
    }

    # Perform ANOVA with error handling
    tryCatch({
      fit <- stats::aov(formula, data = df)
      anova_result <- stats::anova(fit)
      # Extract p-value for batch effect (first row)
      p_value <- anova_result$`Pr(>F)`[1]
      return(p_value)
    }, error = function(e) {
      # Return NA_real_ if ANOVA fails
      return(NA_real_)
    })
  }

  # Apply ANOVA to each variable using purrr
  cli::cli_alert_info("Detecting batch effects using ANOVA for {n_variables} variables...")

  p_values <- purrr::map_dbl(1:n_variables, ~ perform_anova(expr_mat[.x, ]))

  # Set names for the p-values vector
  names(p_values) <- rownames(expr_mat)

  # Report results
  significant_vars <- sum(p_values < 0.05, na.rm = TRUE)
  cli::cli_alert_success(
    "Batch effect detection completed. {significant_vars} out of {n_variables} variables show significant batch effects (p < 0.05)."
  )

  return(p_values)
}
