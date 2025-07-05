#' Adjust Protein Expression
#'
#' This function adjusts the glycopeptide expression by the protein expression.
#' In another word, it "strips out" the protein expression from the glycopeptide expression,
#' so that the expression reflects the glycosylation status only.
#'
#' @details
#'
#' For simplicity, glycopeptide expression is denoted as `GP`,
#' and protein expression is denoted as `PRO`.
#'
#' ## "ratio" method
#'
#' `GP-adj` = (`GP` / `PRO`) / (median(`GP`) / median(`PRO`))
#'
#' The first part is to adjust glycopeptide expression by protein expression.
#' The second part is to rescale the expression to the original scale.
#'
#' ## "reg" method
#'
#' Use linear regression to remove the protein expression from the glycopeptide expression.
#' Both glycopeptide and protein expression values are log2-transformed (with +1 to avoid log(0))
#' before fitting the linear model: log2(GP+1) ~ log2(PRO+1).
#' The residuals represent the glycosylation-specific signal and are converted back to the 
#' original scale using 2^residuals, ensuring all adjusted values are positive.
#'
#' In both methods, only glycoproteins identified in both `exp` and `pro_expr_mat` will be retained.
#'
#' @param exp A `glyexp_experiment` object containing glycoproteomics data.
#' @param pro_expr_mat A matrix of protein expression.
#'  Columns are samples, rows are uniprot protein accessions.
#' @param method The method to use for protein adjustment. 
#'  Either "ratio" or "reg". Default is "ratio".
#'
#' @return A `glyexp_experiment` object with adjusted protein expression.
#' @export
#' 
#' @importFrom rlang .data
adjust_protein <- function(exp, pro_expr_mat, method = c("ratio", "reg")) {
  .dispatch_on_input(
    exp,
    fun_exp = .adjust_protein_experiment,
    fun_mat = .adjust_protein_matrix,
    pro_expr_mat = pro_expr_mat,
    method = method
  )
}

.adjust_protein_experiment <- function(exp, pro_expr_mat, method = c("ratio", "reg")) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  method <- rlang::arg_match(method)

  # Check if the protein column exists
  if (!"protein" %in% colnames(exp$var_info)) {
    cli::cli_abort("The {.field protein} column does not exist.")
  }

  # Filter variables
  common_pro <- intersect(exp$var_info$protein, rownames(pro_expr_mat))
  if (length(common_pro) == 0) {
    cli::cli_abort(c(
      "No common proteins found between {.var exp} and {.var pro_expr_mat}.",
      "i" = "Please check if the protein accessions are correct. It should be something like 'P08185'."
    ))
  }

  original_n_vars <- nrow(exp$var_info)

  exp <- glyexp::filter_var(exp, .data$protein %in% common_pro)
  pro_expr_mat <- pro_expr_mat[common_pro, , drop = FALSE]

  new_exp <- switch(
    method,
    ratio = .adjust_protein_ratio(exp, pro_expr_mat),
    reg = .adjust_protein_reg(exp, pro_expr_mat)
  )

  n_dropped <- original_n_vars - nrow(new_exp$var_info)
  if (n_dropped > 0) {
    cli::cli_alert_info("Dropped {n_dropped} glycopeptides because they are not present in the protein expression matrix.")
  }

  new_exp
}

.adjust_protein_matrix <- function(exp, pro_expr_mat, method = c("ratio", "reg")) {
  cli::cli_abort("The {.fn adjust_protein} function does not support matrix input.")
}

.adjust_protein_ratio <- function(exp, pro_expr_mat) {
  # Get the glycopeptide expression matrix
  gp_expr_mat <- exp$expr_mat
  
  # Ensure sample order consistency
  common_samples <- intersect(colnames(gp_expr_mat), colnames(pro_expr_mat))
  if (length(common_samples) == 0) {
    cli::cli_abort(c(
      "No common samples found between expression matrices.",
      "i" = "Please check if the sample names are correct."
    ))
  }
  
  # Subset and reorder to match samples
  gp_expr_mat <- gp_expr_mat[, common_samples, drop = FALSE]
  pro_expr_mat <- pro_expr_mat[, common_samples, drop = FALSE]
  
  # Apply ratio method for each glycopeptide using purrr
  adjusted_rows <- purrr::imap(seq_len(nrow(gp_expr_mat)), function(i, idx) {
    var_name <- rownames(gp_expr_mat)[i]
    protein <- exp$var_info$protein[exp$var_info$variable == var_name]
    gp_values <- gp_expr_mat[i, ]
    pro_values <- pro_expr_mat[protein, ]
    ratio_values <- gp_values / pro_values
    scaling_factor <- stats::median(gp_values, na.rm = TRUE) / stats::median(pro_values, na.rm = TRUE)
    ratio_values / scaling_factor
  })
  
  # Reconstruct the matrix
  adjusted_expr_mat <- do.call(rbind, adjusted_rows)
  rownames(adjusted_expr_mat) <- rownames(gp_expr_mat)
  colnames(adjusted_expr_mat) <- colnames(gp_expr_mat)
  
  # Update the expression matrix in the experiment object
  new_exp <- exp
  new_exp$expr_mat <- adjusted_expr_mat
  
  # Update sample_info to keep only common samples
  new_exp$sample_info <- exp$sample_info[exp$sample_info$sample %in% common_samples, ]
  
  new_exp
}

.adjust_protein_reg <- function(exp, pro_expr_mat) {
  # Get the glycopeptide expression matrix
  gp_expr_mat <- exp$expr_mat
  
  # Ensure sample order consistency
  common_samples <- intersect(colnames(gp_expr_mat), colnames(pro_expr_mat))
  if (length(common_samples) == 0) {
    cli::cli_abort(c(
      "No common samples found between expression matrices.",
      "i" = "Please check if the sample names are correct."
    ))
  }
  
  # Subset and reorder to match samples
  gp_expr_mat <- gp_expr_mat[, common_samples, drop = FALSE]
  pro_expr_mat <- pro_expr_mat[, common_samples, drop = FALSE]
  
  # Apply regression method for each glycopeptide using purrr
  adjusted_rows <- purrr::imap(seq_len(nrow(gp_expr_mat)), function(i, idx) {
    var_name <- rownames(gp_expr_mat)[i]
    protein <- exp$var_info$protein[exp$var_info$variable == var_name]
    
    gp_values <- gp_expr_mat[i, ]
    pro_values <- pro_expr_mat[protein, ]
    
    # Remove samples with NA values for both GP and PRO
    valid_samples <- !is.na(gp_values) & !is.na(pro_values)
    
    if (sum(valid_samples) < 3) {
      # Not enough valid samples for regression, keep original values
      cli::cli_warn("Variable {.field {var_name}} has fewer than 3 valid samples. Keeping original values.")
      return(gp_values)
    }
    
    # Log2 transform values before regression (add 1 to avoid log(0))
    log_gp_values <- log2(gp_values[valid_samples] + 1)
    log_pro_values <- log2(pro_values[valid_samples] + 1)
    
    # Fit linear regression: log2(GP) ~ log2(PRO)
    model_data <- data.frame(
      log_gp = log_gp_values,
      log_pro = log_pro_values
    )
    
    fit <- tryCatch({
      stats::lm(log_gp ~ log_pro, data = model_data)
    }, error = function(e) {
      cli::cli_warn("Regression failed for variable {.field {var_name}}. Keeping original values.")
      return(NULL)
    })
    
    if (!is.null(fit)) {
      # Use residuals as adjusted values for valid samples
      # Residuals represent the glycosylation-specific signal in log2 space
      # Convert back to original scale using 2^residuals (always positive)
      residuals <- stats::residuals(fit)
      
      # Create adjusted values vector
      adjusted_values <- gp_values
      adjusted_values[valid_samples] <- 2^residuals
      adjusted_values[!valid_samples] <- NA
      
      return(adjusted_values)
    } else {
      return(gp_values)
    }
  })
  
  # Reconstruct the matrix
  adjusted_expr_mat <- do.call(rbind, adjusted_rows)
  rownames(adjusted_expr_mat) <- rownames(gp_expr_mat)
  colnames(adjusted_expr_mat) <- colnames(gp_expr_mat)
  
  # Update the expression matrix in the experiment object
  new_exp <- exp
  new_exp$expr_mat <- adjusted_expr_mat
  
  # Update sample_info to keep only common samples
  new_exp$sample_info <- exp$sample_info[exp$sample_info$sample %in% common_samples, ]
  
  new_exp
}
