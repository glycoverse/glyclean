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
#' The second part normalizes the ratios by the ratio of the row medians.
#' The result is a normalized ratio, not an absolute abundance.
#'
#' ## "reg" method
#'
#' Use linear regression to remove the protein expression from the glycopeptide expression.
#' Both glycopeptide and protein expression values are log2-transformed (with +1 to avoid log(0))
#' before fitting the linear model: log2(GP+1) ~ log2(PRO+1).
#' The residuals represent the glycosylation-specific signal and are converted back to the
#' abundance domain using 2^residuals, giving positive exponentiated residuals.
#' Rows with fewer than three paired observations or failed fits retain their
#' original glycopeptide values.
#'
#' In both methods, only glycoproteins and samples identified in both `exp` and
#' `pro_expr_mat` are retained, with samples aligned by name.
#'
#' Both inputs may independently use ordinary `raw`, `log` (natural logarithm),
#' `log2`, or `log10` representations. Logarithms are decoded to raw abundances
#' before applying the formulas above; these are ordinary logarithms, not
#' `log2(x + 1)` working values. Results use the original representation and
#' scale metadata of `exp`. An absent experiment `scale` field is treated as
#' `"raw"`; explicitly invalid values are rejected. For older SE inputs without
#' this field, the result records `scale = "raw"` so it remains a valid object.
#' The `logx` label is rejected for either input because
#' it does not identify an invertible abundance transformation.
#'
#' Abundances must be nonnegative and finite or missing. Negative log values
#' represent abundances below one and are allowed; `-Inf` represents raw zero.
#' Log values that overflow or underflow when decoded to raw are rejected.
#' The ratio method preserves ordinary division behavior at zero denominators
#' or zero medians, including non-finite results; it does not impute zeros.
#'
#' @param exp A [glyexp::GlycoproteomicSE()] object.
#' @param pro_expr_mat A matrix of protein expression.
#'  Columns are named samples, rows are named UniProt protein accessions that
#'  match the `protein` annotation in `exp`.
#' @param method The method to use for protein adjustment.
#'  Either "ratio" or "reg". Default is "ratio".
#' @param pro_scale Representation of `pro_expr_mat`, independently of the scale
#'   metadata of `exp`: `"raw"` (default), `"log"`, `"log2"`, or `"log10"`.
#'
#' @return A [glyexp::GlycoproteomicSE()] object with adjusted protein
#'   expression, represented on the input scale of `exp`.
#' @examples
#' exp <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
#' proteins <- unique(SummarizedExperiment::rowData(exp)$protein)
#' pro <- matrix(1, length(proteins), ncol(exp),
#'   dimnames = list(proteins, colnames(exp)))
#' adjusted <- adjust_protein(exp, log2(pro), pro_scale = "log2")
#' @export
#'
#' @importFrom rlang .data
adjust_protein <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
) {
  UseMethod("adjust_protein")
}

#' @rdname adjust_protein
#' @export
adjust_protein.glyexp_experiment <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
) {
  .adjust_protein_experiment(
    exp,
    pro_expr_mat = pro_expr_mat,
    method = method,
    pro_scale = pro_scale,
    error_call = quote(adjust_protein())
  )
}

#' @export
#' @noRd
adjust_protein.GlycomicSE <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
) {
  .adjust_protein_experiment(
    exp,
    pro_expr_mat = pro_expr_mat,
    method = method,
    pro_scale = pro_scale,
    error_call = quote(adjust_protein())
  )
}

#' @export
#' @noRd
adjust_protein.GlycoproteomicSE <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
) {
  .adjust_protein_experiment(
    exp,
    pro_expr_mat = pro_expr_mat,
    method = method,
    pro_scale = pro_scale,
    error_call = quote(adjust_protein())
  )
}

#' @rdname adjust_protein
#' @export
adjust_protein.default <- function(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
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
  pro_scale = "raw",
  error_call = rlang::caller_call()
) {
  record <- .pipeline_recorder(exp)
  # Check arguments
  .assert_glycoproteomics_container(exp, error_call = error_call)
  method <- rlang::arg_match(method)
  pro_scale <- .protein_scale(pro_scale, "pro_scale")
  exp_scale <- .protein_scale(.get_scale(exp), "exp scale")
  checkmate::assert_matrix(pro_expr_mat, mode = "numeric")
  checkmate::assert_character(
    rownames(pro_expr_mat),
    any.missing = FALSE,
    unique = TRUE,
    min.len = 1
  )
  checkmate::assert_character(
    colnames(pro_expr_mat),
    any.missing = FALSE,
    unique = TRUE,
    min.len = 1
  )

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
    ratio = .adjust_protein_ratio(exp, pro_expr_mat, exp_scale, pro_scale),
    reg = .adjust_protein_reg(exp, pro_expr_mat, exp_scale, pro_scale)
  )

  n_dropped <- original_n_vars - nrow(.get_var_info(new_exp))
  if (n_dropped > 0) {
    cli::cli_alert_info(
      "Dropped {n_dropped} glycopeptides because they are not present in the protein expression matrix."
    )
  }

  record(
    new_exp,
    .new_pipeline_step(
      adjust_protein,
      list(
        pro_expr_mat = pro_expr_mat,
        method = method,
        pro_scale = pro_scale
      )
    )
  )
}

.adjust_protein_ratio <- function(exp, pro_expr_mat, exp_scale, pro_scale) {
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
  gp_expr_mat <- .protein_to_raw(gp_expr_mat, exp_scale, "exp")
  pro_expr_mat <- .protein_to_raw(pro_expr_mat, pro_scale, "pro_expr_mat")

  # Apply ratio method for each glycopeptide using purrr
  adjusted_rows <- purrr::imap(seq_len(nrow(gp_expr_mat)), function(i, idx) {
    var_name <- rownames(gp_expr_mat)[i]
    protein <- var_info$protein[[i]]
    gp_values <- gp_expr_mat[i, ]
    pro_values <- pro_expr_mat[protein, ]
    ratio_values <- gp_values / pro_values
    scaling_factor <- stats::median(gp_values, na.rm = TRUE) /
      stats::median(pro_values, na.rm = TRUE)
    .protein_from_raw(ratio_values / scaling_factor, exp_scale)
  })

  # Reconstruct the matrix
  adjusted_expr_mat <- do.call(rbind, adjusted_rows)
  rownames(adjusted_expr_mat) <- rownames(gp_expr_mat)
  colnames(adjusted_expr_mat) <- colnames(gp_expr_mat)

  .replace_protein_adjusted_matrix(exp, adjusted_expr_mat, common_samples)
}

.adjust_protein_reg <- function(exp, pro_expr_mat, exp_scale, pro_scale) {
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
  original_gp <- gp_expr_mat
  gp_expr_mat <- .protein_to_raw(gp_expr_mat, exp_scale, "exp")
  pro_expr_mat <- .protein_to_raw(pro_expr_mat, pro_scale, "pro_expr_mat")

  # Apply regression method for each glycopeptide using purrr
  adjusted_rows <- purrr::imap(seq_len(nrow(gp_expr_mat)), function(i, idx) {
    var_name <- rownames(gp_expr_mat)[i]
    protein <- var_info$protein[[i]]

    gp_values <- gp_expr_mat[i, ]
    pro_values <- pro_expr_mat[protein, ]

    # Remove samples with NA values for both GP and PRO
    valid_samples <- !is.na(gp_values) & !is.na(pro_values)

    if (sum(valid_samples) < 3) {
      # Not enough valid samples for regression, keep original values
      cli::cli_warn(
        "Variable {.field {var_name}} has fewer than 3 valid samples. Keeping original values."
      )
      return(original_gp[i, ])
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
      # Exponentiate residuals to the abundance domain (always positive)
      residuals <- stats::residuals(fit)

      # Create adjusted values vector
      adjusted_values <- gp_values
      adjusted_values[valid_samples] <- 2^residuals
      adjusted_values[!valid_samples] <- NA

      return(.protein_from_raw(adjusted_values, exp_scale))
    } else {
      return(original_gp[i, ])
    }
  })

  # Reconstruct the matrix
  adjusted_expr_mat <- do.call(rbind, adjusted_rows)
  rownames(adjusted_expr_mat) <- rownames(gp_expr_mat)
  colnames(adjusted_expr_mat) <- colnames(gp_expr_mat)

  .replace_protein_adjusted_matrix(exp, adjusted_expr_mat, common_samples)
}

# Protein-specific conversions are kept local so the two inputs remain independent.
.protein_scale <- function(scale, arg) {
  if (identical(scale, "logx")) {
    cli::cli_abort(
      "{.arg {arg}} cannot be {.val logx}: raw abundances cannot be recovered from this label."
    )
  }
  rlang::arg_match0(scale, c("raw", "log", "log2", "log10"), arg_nm = arg)
}

.protein_to_raw <- function(x, scale, arg) {
  raw <- switch(scale, raw = x, log = exp(x), log2 = 2^x, log10 = 10^x)
  if (any(!is.na(raw) & (!is.finite(raw) | raw < 0))) {
    cli::cli_abort(
      "{.arg {arg}} must decode to finite, nonnegative raw abundances or missing values."
    )
  }
  if (scale != "raw" && any(is.finite(x) & raw == 0, na.rm = TRUE)) {
    cli::cli_abort(
      "{.arg {arg}} contains log values that underflow when decoded to raw abundances."
    )
  }
  raw
}

.protein_from_raw <- function(x, scale) {
  switch(scale, raw = x, log = log(x), log2 = log2(x), log10 = log10(x))
}

# Preserve row annotations and optional/duplicated identifiers by position.
.replace_protein_adjusted_matrix <- function(exp, expr_mat, samples) {
  sample_positions <- match(samples, colnames(.get_expr_mat(exp)))
  result <- .subset_container(exp, cols = sample_positions)
  if (inherits(result, "glyexp_experiment")) {
    result$expr_mat <- expr_mat
  } else {
    SummarizedExperiment::assay(result, 1) <- expr_mat
    result <- .ensure_scale_metadata(result)
    methods::validObject(result)
  }
  result
}
