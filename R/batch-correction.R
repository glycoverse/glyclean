# Batch effect correction functions

#' Correct Batch Effect
#'
#' Correct batch effects in glycoproteomics/glycomics data using ComBat algorithm
#' from the sva package or removeBatchEffect from the limma package.
#'
#' @details
#' This function performs batch effect correction using either the ComBat algorithm
#' or the limma removeBatchEffect function. It requires batch information provided
#' via the `batch` parameter. If no batch information is available, the function
#' will return the original data unchanged.
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
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param batch Either a factor/character vector specifying batch assignments for each sample,
#'   or a string specifying the column name in `sample_info`. Defaults to "batch".
#' @param group Either a factor/character vector specifying group assignments for each sample,
#'   or a string specifying the column name in `sample_info`.
#'   If provided, it will be used as a covariate in the batch correction model.
#'   This is useful when you have an unbalanced design.
#'   Default to NULL.
#' @param check_confounding Whether to check for confounding between batch and group variables.
#'   Default to TRUE.
#' @param confounding_threshold The threshold for Cramer's V to consider batch and group variables highly confounded.
#'   Only used when `check_confounding` is TRUE. Default to 0.4.
#' @param method The batch correction method to use. Either "combat" (default, uses
#'   sva::ComBat) or "limma" (uses limma::removeBatchEffect). Default to "combat".
#'
#' @return A container of the same class as `x`, with batch effects corrected.
#'
#' @examples
#' library(SummarizedExperiment)
#'
#' # With a SummarizedExperiment and column names
#' exp <- glyexp::real_experiment
#' batch <- rep(c("A", "B"), length.out = ncol(exp))
#' group <- rep(c("Ctrl", "Ctrl", "Treat", "Treat"), length.out = ncol(exp))
#' colData(exp)$batch <- batch
#' colData(exp)$group <- group
#' corrected_exp <- correct_batch_effect(exp, batch = "batch", group = "group")
#'
#' # Using limma method
#' corrected_exp <- correct_batch_effect(
#'   exp, batch = "batch", group = "group", method = "limma"
#' )
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
  .assert_glyclean_container(x)
  rlang::check_installed("sva", reason = "to use `correct_batch_effect()`")
  method <- match.arg(method)

  batch_group_info <- .extract_batch_group_from_experiment(
    x,
    batch,
    group,
    require_batch = TRUE
  )
  if (is.null(batch_group_info)) {
    cli::cli_alert_info(
      "No batch information found in column '{batch}' of sample_info. Returning original experiment unchanged."
    )
    return(x)
  }

  corrected_expr_mat <- .apply_batch_correction(
    .get_expr_mat(x),
    batch_group_info$batch,
    batch_group_info$group,
    check_confounding = check_confounding,
    confounding_threshold = confounding_threshold,
    method = method
  )

  if (is.null(corrected_expr_mat)) {
    return(x)
  }

  if (!inherits(x, "glyexp_experiment")) {
    corrected_expr_mat[corrected_expr_mat < 0] <- 0
  }

  .rebuild_container(x, expr_mat = corrected_expr_mat)
}


#' Detect batch effect
#'
#' Use ANOVA to detect if batch effect is present in the data.
#' If `group` is provided, it will be used as a covariate in the ANOVA model.
#'
#' @param x A [glyexp::GlycomicSE()], [glyexp::GlycoproteomicSE()], or
#'   [SummarizedExperiment::SummarizedExperiment()] object.
#' @param batch Either a factor/character vector specifying batch assignments for each sample,
#'   or a string specifying the column name in `sample_info`. Defaults to "batch".
#' @param group Either a factor/character vector specifying group assignments for each sample,
#'   or a string specifying the column name in `sample_info`.
#'   If provided, it will be used as a covariate in the ANOVA model.
#'   This is useful when you have an unbalanced design.
#'   Default to NULL.
#'
#' @returns A double vector of p-values for each variable, with the same length
#'   as `nrow(x)`.
#'
#' @examples
#' library(SummarizedExperiment)
#'
#' # With a SummarizedExperiment and column names
#' exp <- glyexp::real_experiment
#' batch <- rep(c("A", "B"), length.out = ncol(exp))
#' group <- rep(c("Ctrl", "Ctrl", "Treat", "Treat"), length.out = ncol(exp))
#' colData(exp)$batch <- batch
#' colData(exp)$group <- group
#' p_values <- detect_batch_effect(exp, batch = "batch", group = "group")
#'
#' @export
detect_batch_effect <- function(x, batch = "batch", group = NULL) {
  .assert_glyclean_container(x)

  batch_group_info <- .extract_batch_group_from_experiment(
    x,
    batch,
    group,
    require_batch = TRUE
  )
  if (is.null(batch_group_info)) {
    return(rep(1, nrow(.get_expr_mat(x))))
  }

  .perform_batch_effect_detection(
    .get_expr_mat(x),
    batch_group_info$batch,
    batch_group_info$group
  )
}

# Helper functions for common business logic

.extract_batch_group_from_experiment <- function(
  x,
  batch,
  group,
  require_batch = FALSE
) {
  # Validate experiment input
  .assert_glyclean_container(x)
  sample_info <- .get_sample_info(x)
  expr_mat <- .get_expr_mat(x)

  # Handle batch parameter with special logic for non-required cases
  batch_values <- NULL
  if (is.character(batch) && length(batch) == 1) {
    # For string input, check if column exists first when not required
    if (batch %in% colnames(sample_info)) {
      batch_values <- .resolve_column_param(
        batch,
        sample_info = sample_info,
        param_name = "batch",
        n_samples = ncol(expr_mat),
        allow_null = FALSE
      )
    } else if (require_batch) {
      # If batch is required but column doesn't exist, let resolve_column_param handle the error
      batch_values <- .resolve_column_param(
        batch,
        sample_info = sample_info,
        param_name = "batch",
        n_samples = ncol(expr_mat),
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
      sample_info = sample_info,
      param_name = "batch",
      n_samples = ncol(expr_mat),
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
    sample_info = sample_info,
    param_name = "group",
    n_samples = ncol(expr_mat),
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

.check_batch_group_confounding <- function(batch, group, threshold = 0.4) {
  confusion_table <- table(batch, group)
  abs(.cramers_v(confusion_table)) > threshold
}

.cramers_v <- function(conf_table) {
  statistic <- as.numeric(
    suppressWarnings(stats::chisq.test(conf_table))$statistic
  )
  sqrt((statistic / sum(conf_table)) / min(dim(conf_table) - 1))
}

.has_enough_samples_per_batch <- function(batch) {
  batch_counts <- table(batch)
  all(batch_counts >= 2)
}

.apply_batch_correction <- function(
  expr_mat,
  batch,
  group = NULL,
  check_confounding = TRUE,
  confounding_threshold = 0.4,
  method = c("combat", "limma")
) {
  method <- match.arg(method)

  # Check if there are at least 2 batches
  if (length(unique(batch)) < 2) {
    cli::cli_warn(
      "Less than 2 batches found. Batch correction requires at least 2 batches. Returning original data unchanged."
    )
    return(expr_mat)
  }

  # Check for confounding
  if (
    check_confounding &&
      !is.null(group) &&
      .check_batch_group_confounding(batch, group, confounding_threshold)
  ) {
    cli::cli_warn(c(
      "Batch and group variables are highly confounded.",
      "i" = "Batch effect correction may not be appropriate.",
      "i" = "Returning original data unchanged."
    ))
    return(expr_mat)
  }

  # Check sufficient samples per batch (only for ComBat)
  if (method == "combat" && !.has_enough_samples_per_batch(batch)) {
    cli::cli_warn(c(
      "Some batches have fewer than 2 samples.",
      "i" = "ComBat requires at least 2 samples per batch.",
      "i" = "Returning original data unchanged."
    ))
    return(expr_mat)
  }

  # Log transform
  log_expr_mat <- log2(expr_mat + 1e-6)

  # Create model matrix
  if (!is.null(group)) {
    mod <- stats::model.matrix(~group)
  } else {
    mod <- NULL
  }

  # Apply batch correction based on method
  if (method == "combat") {
    corrected_log_expr_mat <- tryCatch(
      {
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
      },
      error = function(e) {
        cli::cli_warn(c(
          "ComBat failed to correct batch effects.",
          "i" = "Error: {e$message}",
          "i" = "Returning original data unchanged."
        ))
        return(NULL)
      }
    )
  } else {
    # limma method
    corrected_log_expr_mat <- tryCatch(
      {
        suppressWarnings(limma::removeBatchEffect(
          log_expr_mat,
          batch = batch,
          covariates = mod
        ))
      },
      error = function(e) {
        cli::cli_warn(c(
          "limma removeBatchEffect failed to correct batch effects.",
          "i" = "Error: {e$message}",
          "i" = "Returning original data unchanged."
        ))
        return(NULL)
      }
    )
  }

  # Check if correction succeeded
  if (is.null(corrected_log_expr_mat)) {
    return(expr_mat)
  }

  # Convert back from log space
  2^corrected_log_expr_mat - 1e-6
}

.perform_batch_effect_detection <- function(expr_mat, batch, group = NULL) {
  # Check if there are at least 2 batches
  if (length(unique(batch)) < 2) {
    cli::cli_warn(
      "Less than 2 batches found. Cannot perform batch effect detection."
    )
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
    tryCatch(
      {
        fit <- stats::aov(formula, data = df)
        anova_result <- stats::anova(fit)
        # Extract p-value for batch effect (first row)
        p_value <- anova_result$`Pr(>F)`[1]
        return(p_value)
      },
      error = function(e) {
        # Return NA_real_ if ANOVA fails
        return(NA_real_)
      }
    )
  }

  # Apply ANOVA to each variable using purrr
  cli::cli_alert_info(
    "Detecting batch effects using ANOVA for {n_variables} variables..."
  )

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
