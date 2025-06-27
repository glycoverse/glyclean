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
#' If batch and group are highly confounded (complete overlap),
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
#'
#' @return For `glyexp_experiment` input, returns a modified `glyexp_experiment` object.
#'   For matrix input, returns a batch-corrected matrix.
#' 
#' @examples
#' # With glyexp_experiment and column names
#' exp <- glyexp::toy_experiment()
#' exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
#' exp$sample_info$group <- c("Ctrl", "Ctrl", "Treat", "Ctrl", "Treat", "Treat")
#' corrected_exp <- correct_batch_effect(exp, batch = "batch", group = "group")
#' 
#' # With matrix and factor vectors
#' mat <- matrix(rnorm(200), nrow = 20, ncol = 10)
#' batch_factor <- factor(rep(c("A", "B"), each = 5))
#' group_factor <- factor(rep(c("Ctrl", "Treat"), times = 5))
#' corrected_mat <- correct_batch_effect(mat, batch = batch_factor, group = group_factor)
#' 
#' @importFrom utils capture.output
#' @export
correct_batch_effect <- function(x, batch = "batch", group = NULL) {
  
  # Handle different input types
  if (is.matrix(x)) {
    return(.correct_batch_effect_matrix(x, batch = batch, group = group))
  }
  
  # Validate experiment input
  checkmate::assert_class(x, "glyexp_experiment")
  
  # For experiment input, batch and group should be column names
  checkmate::assert_string(batch)
  checkmate::assert_string(group, null.ok = TRUE)
  
  # Get sample information
  sample_info <- x$sample_info
  
  # Check if batch column exists
  if (!batch %in% colnames(sample_info)) {
    cli::cli_alert_info("No batch information found in column '{batch}' of sample_info. Returning original experiment unchanged.")
    return(x)
  }
  
  # Get batch and group information
  batch_values <- sample_info[[batch]]
  has_group <- !is.null(group) && group %in% colnames(sample_info)
  
  # Check for confounding between batch and group if both exist
  if (has_group) {
    group_values <- sample_info[[group]]
    
    # Create a contingency table to check for confounding
    confusion_table <- table(batch_values, group_values)
    
    # Check if batch and group are highly confounded
    # If each batch has only one group, they are perfectly confounded
    batch_group_overlap <- all(rowSums(confusion_table > 0) == 1)
    
    if (batch_group_overlap) {
      cli::cli_warn(c(
        "Batch and group variables are highly confounded.",
        "i" = "Each batch contains only one group, making batch correction problematic.",
        "i" = "Returning original experiment unchanged to avoid over-correction."
      ))
      return(x)
    }
  }

  # Perform batch correction using ComBat
  expr_mat <- x$expr_mat
  log_expr_mat <- log2(expr_mat + 1)
  
  # Check if there are enough samples per batch for ComBat
  batch_counts <- table(batch_values)
  if (any(batch_counts < 2)) {
    cli::cli_warn(c(
      "Some batches have fewer than 2 samples.",
      "i" = "ComBat requires at least 2 samples per batch.",
      "i" = "Returning original experiment unchanged."
    ))
    return(x)
  }
  
  # Create model matrix
  if (has_group) {
    # Include group in the model to preserve biological variation
    group_values <- sample_info[[group]]
    mod <- stats::model.matrix(~ group_values)
  } else {
    # No covariates to preserve
    mod <- NULL
  }
  
  # Apply ComBat correction with error handling and suppressed output
  corrected_log_expr_mat <- tryCatch({
    # Suppress ComBat's verbose output completely
    suppressMessages({
      capture.output({
        combat_result <- sva::ComBat(
          dat = log_expr_mat,
          batch = batch_values,
          mod = mod,
          par.prior = TRUE,
          prior.plots = FALSE
        )
      }, file = nullfile())
      combat_result
    })
  }, error = function(e) {
    cli::cli_warn(c(
      "ComBat failed to correct batch effects.",
      "i" = "Error: {e$message}",
      "i" = "Returning original experiment unchanged."
    ))
    return(NULL)
  })
  
  # Check if ComBat succeeded
  if (is.null(corrected_log_expr_mat)) {
    return(x)
  }

  # Update experiment with corrected expression matrix
  new_exp <- x
  new_exp$expr_mat <- 2^corrected_log_expr_mat - 1
  
  cli::cli_alert_success("Batch effect correction completed using ComBat algorithm.")
  
  return(new_exp)
}

.correct_batch_effect_matrix <- function(x, batch, group = NULL) {
  # Validate matrix input
  checkmate::assert_matrix(x)
  
  # For matrix input, batch and group should be vectors
  checkmate::assert_vector(batch, len = ncol(x))
  if (!is.null(group)) {
    checkmate::assert_vector(group, len = ncol(x))
  }
  
  # Convert to factors if needed
  batch <- factor(batch)
  if (!is.null(group)) {
    group <- factor(group)
  }
  
  # Check for confounding between batch and group if both exist
  if (!is.null(group)) {
    # Create a contingency table to check for confounding
    confusion_table <- table(batch, group)
    
    # Check if batch and group are highly confounded
    batch_group_overlap <- all(rowSums(confusion_table > 0) == 1)
    
    if (batch_group_overlap) {
      cli::cli_warn(c(
        "Batch and group variables are highly confounded.",
        "i" = "Each batch contains only one group, making batch correction problematic.",
        "i" = "Returning original matrix unchanged to avoid over-correction."
      ))
      return(x)
    }
  }
  
  # Perform batch correction using ComBat
  log_expr_mat <- log2(x + 1)
  
  # Check if there are enough samples per batch for ComBat
  batch_counts <- table(batch)
  if (any(batch_counts < 2)) {
    cli::cli_warn(c(
      "Some batches have fewer than 2 samples.",
      "i" = "ComBat requires at least 2 samples per batch.",
      "i" = "Returning original matrix unchanged."
    ))
    return(x)
  }
  
  # Create model matrix
  if (!is.null(group)) {
    # Include group in the model to preserve biological variation
    mod <- stats::model.matrix(~ group)
  } else {
    # No covariates to preserve
    mod <- NULL
  }
  
  # Apply ComBat correction with error handling and suppressed output
  corrected_log_expr_mat <- tryCatch({
    # Suppress ComBat's verbose output completely
    suppressMessages({
      capture.output({
        combat_result <- sva::ComBat(
          dat = log_expr_mat,
          batch = batch,
          mod = mod,
          par.prior = TRUE,
          prior.plots = FALSE
        )
      }, file = nullfile())
      combat_result
    })
  }, error = function(e) {
    cli::cli_warn(c(
      "ComBat failed to correct batch effects.",
      "i" = "Error: {e$message}",
      "i" = "Returning original matrix unchanged."
    ))
    return(NULL)
  })
  
  # Check if ComBat succeeded
  if (is.null(corrected_log_expr_mat)) {
    return(x)
  }
  
  # Convert back from log space
  corrected_expr_mat <- 2^corrected_log_expr_mat - 1
  
  cli::cli_alert_success("Batch effect correction completed using ComBat algorithm.")
  
  return(corrected_expr_mat)
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
#' exp <- glyexp::toy_experiment()
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
  
  # Handle different input types
  if (is.matrix(x)) {
    return(.detect_batch_effect_matrix(x, batch = batch, group = group))
  }
  
  # Validate experiment input
  checkmate::assert_class(x, "glyexp_experiment")
  
  # For experiment input, batch and group should be column names
  checkmate::assert_string(batch)
  checkmate::assert_string(group, null.ok = TRUE)
  
  # Get sample information and expression matrix
  sample_info <- x$sample_info
  expr_mat <- x$expr_mat
  
  # Check if batch column exists
  if (!batch %in% colnames(sample_info)) {
    cli::cli_abort("Batch column '{batch}' not found in sample_info.")
  }
  
  # Check if group column exists (if provided)
  if (!is.null(group) && !group %in% colnames(sample_info)) {
    cli::cli_abort("Group column '{group}' not found in sample_info.")
  }
  
  # Get batch information
  batch_values <- sample_info[[batch]]
  group_values <- if (!is.null(group)) sample_info[[group]] else NULL
  
  # Check if there are at least 2 batches
  if (length(unique(batch_values)) < 2) {
    cli::cli_warn("Less than 2 batches found. Cannot perform batch effect detection.")
    return(rep(1, nrow(expr_mat)))
  }
  
  # Prepare data for ANOVA
  # Each row of expr_mat is a variable, each column is a sample
  n_variables <- nrow(expr_mat)
  n_samples <- ncol(expr_mat)
  
  # Function to perform ANOVA for a single variable
  perform_anova <- function(variable_values) {
    # Create data frame for ANOVA
    df <- data.frame(
      value = as.numeric(variable_values),
      batch = factor(batch_values)
    )
    
    # Add group column if provided
    if (!is.null(group_values)) {
      df$group <- factor(group_values)
    }
    
    # Build formula
    if (!is.null(group_values)) {
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

.detect_batch_effect_matrix <- function(x, batch, group = NULL) {
  # Validate matrix input
  checkmate::assert_matrix(x)
  
  # For matrix input, batch and group should be vectors
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
    cli::cli_warn("Less than 2 batches found. Cannot perform batch effect detection.")
    return(rep(1, nrow(x)))
  }
  
  # Prepare data for ANOVA
  # Each row of x is a variable, each column is a sample
  n_variables <- nrow(x)
  n_samples <- ncol(x)
  
  # Function to perform ANOVA for a single variable
  perform_anova <- function(variable_values) {
    # Create data frame for ANOVA
    df <- data.frame(
      value = as.numeric(variable_values),
      batch = batch
    )
    
    # Add group column if provided
    if (!is.null(group)) {
      df$group <- group
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
  
  p_values <- purrr::map_dbl(1:n_variables, ~ perform_anova(x[.x, ]))
  
  # Set names for the p-values vector
  names(p_values) <- rownames(x)
  
  # Report results
  significant_vars <- sum(p_values < 0.05, na.rm = TRUE)
  cli::cli_alert_success(
    "Batch effect detection completed. {significant_vars} out of {n_variables} variables show significant batch effects (p < 0.05)."
  )
  
  return(p_values)
}
