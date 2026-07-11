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
#' @param exp A [glyexp::experiment()] object with "glycoproteomics" type.
#'   [glyexp::GlycoproteomicSE()] objects are also supported.
#' @param pro_expr_mat A matrix of protein expression.
#'  Columns are samples, rows are uniprot protein accessions.
#' @param method The method to use for protein adjustment.
#'  Either "ratio" or "reg". Default is "ratio".
#'
#' @return A [glyexp::experiment()] object with adjusted protein expression. A
#'   [glyexp::GlycoproteomicSE()] input returns the same subclass.
#' @export
#'
#' @importFrom rlang .data
adjust_protein <- function(exp, pro_expr_mat, method = c("ratio", "reg")) {
  UseMethod("adjust_protein")
}

#' @rdname adjust_protein
#' @export
adjust_protein.glyexp_experiment <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg")
) {
  .adjust_protein_experiment(
    exp,
    pro_expr_mat = pro_expr_mat,
    method = method,
    error_call = quote(adjust_protein())
  )
}

#' @export
#' @noRd
adjust_protein.GlycomicSE <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg")
) {
  .adjust_protein_experiment(
    exp,
    pro_expr_mat = pro_expr_mat,
    method = method,
    error_call = quote(adjust_protein())
  )
}

#' @export
#' @noRd
adjust_protein.GlycoproteomicSE <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg")
) {
  .adjust_protein_experiment(
    exp,
    pro_expr_mat = pro_expr_mat,
    method = method,
    error_call = quote(adjust_protein())
  )
}

#' @rdname adjust_protein
#' @export
adjust_protein.default <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg")
) {
  cli::cli_abort(c(
    "{.arg exp} must be a {.cls glyexp_experiment} object or a {.cls GlycoproteomicSE} object.",
    "x" = "Got {.cls {class(exp)}}."
  ))
}

.adjust_protein_experiment <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  error_call = rlang::caller_call()
) {
  # Check arguments
  .assert_glycoproteomics_container(exp, error_call = error_call)
  method <- rlang::arg_match(method)

  # Check if the protein column exists
  var_info <- .get_var_info(exp)
  if (!"protein" %in% colnames(var_info)) {
    cli::cli_abort("The {.field protein} column does not exist.")
  }

  # Filter variables
  common_pro <- intersect(var_info$protein, rownames(pro_expr_mat))
  if (length(common_pro) == 0) {
    cli::cli_abort(c(
      "No common proteins found between {.var exp} and {.var pro_expr_mat}.",
      "i" = "Please check if the protein accessions are correct. It should be something like 'P08185'."
    ))
  }

  original_n_vars <- nrow(var_info)

  exp <- .subset_container(exp, rows = which(var_info$protein %in% common_pro))
  pro_expr_mat <- pro_expr_mat[common_pro, , drop = FALSE]

  new_exp <- switch(
    method,
    ratio = .adjust_protein_ratio(exp, pro_expr_mat),
    reg = .adjust_protein_reg(exp, pro_expr_mat)
  )

  n_dropped <- original_n_vars - nrow(.get_var_info(new_exp))
  if (n_dropped > 0) {
    cli::cli_alert_info(
      "Dropped {n_dropped} glycopeptides because they are not present in the protein expression matrix."
    )
  }

  new_exp
}

.adjust_protein_ratio <- function(exp, pro_expr_mat) {
  # Get the glycopeptide expression matrix
  gp_expr_mat <- .get_expr_mat(exp)
  var_info <- .get_var_info(exp)

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
    protein <- var_info$protein[var_info$variable == var_name]
    gp_values <- gp_expr_mat[i, ]
    pro_values <- pro_expr_mat[protein, ]
    ratio_values <- gp_values / pro_values
    scaling_factor <- stats::median(gp_values, na.rm = TRUE) /
      stats::median(pro_values, na.rm = TRUE)
    ratio_values / scaling_factor
  })

  # Reconstruct the matrix
  adjusted_expr_mat <- do.call(rbind, adjusted_rows)
  rownames(adjusted_expr_mat) <- rownames(gp_expr_mat)
  colnames(adjusted_expr_mat) <- colnames(gp_expr_mat)

  # Update the expression matrix in the experiment object
  sample_info <- .get_sample_info(exp)
  sample_info <- sample_info[
    match(common_samples, sample_info$sample),
    ,
    drop = FALSE
  ]
  .rebuild_container(
    exp,
    expr_mat = adjusted_expr_mat,
    sample_info = sample_info
  )
}

.adjust_protein_reg <- function(exp, pro_expr_mat) {
  # Get the glycopeptide expression matrix
  gp_expr_mat <- .get_expr_mat(exp)
  var_info <- .get_var_info(exp)

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
    protein <- var_info$protein[var_info$variable == var_name]

    gp_values <- gp_expr_mat[i, ]
    pro_values <- pro_expr_mat[protein, ]

    # Remove samples with NA values for both GP and PRO
    valid_samples <- !is.na(gp_values) & !is.na(pro_values)

    if (sum(valid_samples) < 3) {
      # Not enough valid samples for regression, keep original values
      cli::cli_warn(
        "Variable {.field {var_name}} has fewer than 3 valid samples. Keeping original values."
      )
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

    fit <- tryCatch(
      {
        stats::lm(log_gp ~ log_pro, data = model_data)
      },
      error = function(e) {
        cli::cli_warn(
          "Regression failed for variable {.field {var_name}}. Keeping original values."
        )
        return(NULL)
      }
    )

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
  sample_info <- .get_sample_info(exp)
  sample_info <- sample_info[
    match(common_samples, sample_info$sample),
    ,
    drop = FALSE
  ]
  .rebuild_container(
    exp,
    expr_mat = adjusted_expr_mat,
    sample_info = sample_info
  )
}
