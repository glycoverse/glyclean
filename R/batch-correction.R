# Batch effect correction functions

#' Correct Batch Effect
#'
#' Correct batch effects in glycoproteomics/glycomics data using ComBat algorithm
#' from the sva package. This function requires batch information in the sample_info.
#'
#' @param exp A `glyexp_experiment` object containing glycoproteomics/glycomics data.
#'
#' @details
#' This function performs batch effect correction using the ComBat algorithm.
#' It requires a "batch" column in the sample information.
#' If no batch information is available, the function will return the original experiment unchanged.
#' 
#' If a "group" column exists in the sample information,
#' the function will check for confounding between batch and group variables.
#' If batch and group are highly confounded (complete overlap),
#' the function will issue a warning and return the original experiment unchanged
#' to avoid over-correction.
#' 
#' When both batch and group information are available and not highly confounded,
#' the group information will be included in the model to preserve biological variation
#' while correcting for batch effects.
#'
#' @return A `glyexp_experiment` object with batch-corrected expression matrix.
#' 
#' @examples
#' # Create an experiment with batch information
#' exp <- glyexp::toy_experiment()
#' exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
#' exp$sample_info$group <- c("Ctrl", "Ctrl", "Treat", "Ctrl", "Treat", "Treat")
#' 
#' # Correct batch effects
#' corrected_exp <- correct_batch_effect(exp)
#' 
#' @importFrom utils capture.output
#' @export
correct_batch_effect <- function(exp) {
  # Validate input
  checkmate::assert_class(exp, "glyexp_experiment")
  
  # Get sample information
  sample_info <- exp$sample_info
  
  # Check if batch column exists
  if (!"batch" %in% colnames(sample_info)) {
    cli::cli_alert_info("No batch information found in sample_info. Returning original experiment unchanged.")
    return(exp)
  }
  
  # Get batch and group information
  batch <- sample_info$batch
  has_group <- "group" %in% colnames(sample_info)
  
  # Check for confounding between batch and group if both exist
  if (has_group) {
    group <- sample_info$group
    
    # Create a contingency table to check for confounding
    confusion_table <- table(batch, group)
    
    # Check if batch and group are highly confounded
    # If each batch has only one group, they are perfectly confounded
    batch_group_overlap <- all(rowSums(confusion_table > 0) == 1)
    
    if (batch_group_overlap) {
      cli::cli_warn(c(
        "Batch and group variables are highly confounded.",
        "i" = "Each batch contains only one group, making batch correction problematic.",
        "i" = "Returning original experiment unchanged to avoid over-correction."
      ))
      return(exp)
    }
  }
  
  # Perform batch correction using ComBat
  expr_mat <- exp$expr_mat
  log_expr_mat <- log2(expr_mat + 1)
  
  # Check if there are enough samples per batch for ComBat
  batch_counts <- table(batch)
  if (any(batch_counts < 2)) {
    cli::cli_warn(c(
      "Some batches have fewer than 2 samples.",
      "i" = "ComBat requires at least 2 samples per batch.",
      "i" = "Returning original experiment unchanged."
    ))
    return(exp)
  }
  
  # Create model matrix
  if (has_group) {
    # Include group in the model to preserve biological variation
    group <- sample_info$group
    mod <- stats::model.matrix(~ group)
  } else {
    # No covariates to preserve
    mod <- NULL
  }
  
  # Apply ComBat correction with error handling and suppressed output
  corrected_log_expr_mat <- tryCatch({
    # Suppress ComBat's verbose output completely
    suppressMessages({
      capture.output(
        sva::ComBat(
          dat = log_expr_mat,
          batch = batch,
          mod = mod,
          par.prior = TRUE,
          prior.plots = FALSE
        ),
        file = nullfile()
      )
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
    return(exp)
  }
  
  # Update experiment with corrected expression matrix
  new_exp <- exp
  new_exp$expr_mat <- 2^corrected_log_expr_mat - 1
  
  cli::cli_alert_success("Batch effect correction completed using ComBat algorithm.")
  
  return(new_exp)
}
