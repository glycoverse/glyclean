#' Automatic Data Preprocessing
#'
#' Perform automatic data preprocessing on glycoproteomics or glycomics data.
#' This function applies a standardized preprocessing pipeline that includes
#' normalization, missing value handling, imputation, aggregation (for
#' glycoproteomics data), and batch effect correction.
#'
#' @param exp A [glyexp::experiment()] containing glycoproteomics or
#'   glycomics data.
#'
#' @details
#' The preprocessing pipeline differs based on the experiment type:
#' 
#' For Glycoproteomics Data:
#' 
#' 1. Median normalization
#' 2. Remove variables with \\>50% missing values
#' 3. Automatic imputation (method depends on sample size)
#' 4. Automatic aggregation (gfs level if structure available, otherwise gf level)
#' 5. Final median normalization
#' 6. Intelligent batch effect correction
#' 
#' For Glycomics Data:
#' 
#' 1. Median quotient normalization
#' 2. Remove variables with \\>50% missing values  
#' 3. Automatic imputation (method depends on sample size)
#' 4. Total area normalization
#' 5. Intelligent batch effect correction
#' 
#' Automatic Imputation Strategy:
#' 
#' - <=30 samples: Sample minimum imputation
#' - 31-100 samples: Minimum probability imputation
#' - \\>100 samples: MissForest imputation
#' 
#' Automatic Aggregation Strategy (Glycoproteomics Only):
#' 
#' - If `glycan_structure` column exists: Aggregate to "gfs" level
#' - If no `glycan_structure` column: Aggregate to "gf" level
#' 
#' Intelligent Batch Effect Correction:
#' 
#' The function first detects batch effects using ANOVA.
#' Batch correction is only performed if more than 10% of variables 
#' show significant batch effects (p < 0.05).
#' If a `group` column exists in the sample information,
#' it will be used as a covariate in both detection and correction
#' to preserve biological variation.
#' The batches are defined by the `batch` column in the sample information tibble.
#'
#' @importFrom magrittr %>%
#'
#' @return A modified `glyexp::experiment()` object.
#' 
#' @examples
#' \dontrun{
#' # For glycoproteomics data
#' cleaned_exp <- auto_clean(glycoprot_exp)
#' 
#' # For glycomics data
#' cleaned_exp <- auto_clean(glycomics_exp)
#' }
#' 
#' @seealso [aggregate()], [normalize_median()], [remove_rare()],
#'   [impute_sample_min()], [impute_min_prob()], [impute_miss_forest()],
#'   [correct_batch_effect()]
#' @export
auto_clean <- function(exp) {
  checkmate::assert_class(exp, "glyexp_experiment")
  if (!checkmate::test_choice(glyexp::get_exp_type(exp), c("glycoproteomics", "glycomics"))) {
    cli::cli_abort(c(
      "The experiment type must be {.val glycoproteomics} or {.val glycomics}.",
      "x" = "Got {.val {glyexp::get_exp_type(exp)}}."
    ))
  }
  switch(
    glyexp::get_exp_type(exp),
    glycoproteomics = .auto_clean_glycoproteomics(exp),
    glycomics = .auto_clean_glycomics(exp)
  )
}

.auto_clean_glycoproteomics <- function(exp) {
  cli::cli_progress_step("Normalizing data (Median)")
  exp <- normalize_median(exp)
  cli::cli_progress_step("Removing variables with >50% missing values")
  exp <- remove_rare(exp, prop = 0.5)
  cli::cli_progress_step("Imputing missing values")
  exp <- .auto_impute(exp)
  cli::cli_progress_step("Aggregating data")
  exp <- .auto_aggregate(exp)
  cli::cli_progress_step("Normalizing data again")
  exp <- normalize_median(exp)
  exp <- .auto_batch_correct(exp)
  exp
}

.auto_clean_glycomics <- function(exp) {
  cli::cli_progress_step("Normalizing data (Median Quotient)")
  exp <- normalize_median_quotient(exp)
  cli::cli_progress_step("Removing variables with >50% missing values")
  exp <- remove_rare(exp, prop = 0.5)
  cli::cli_progress_step("Imputing missing values")
  exp <- .auto_impute(exp)
  cli::cli_progress_step("Normalizing data (Total Area)")
  exp <- normalize_total_area(exp)
  exp <- .auto_batch_correct(exp)
  exp
}


.auto_impute <- function(exp) {
  if (ncol(exp) <= 30) {
    cli::cli_alert_info("Sample size <= 30, using sample minimum imputation")
    impute_sample_min(exp)
  } else if (ncol(exp) <= 100) {
    cli::cli_alert_info("Sample size <= 100, using minimum probability imputation")
    impute_min_prob(exp)
  } else {
    cli::cli_alert_info("Sample size > 100, using MissForest imputation")
    impute_miss_forest(exp)
  }
}


.auto_aggregate <- function(exp) {
  # Aggregate to "glycoform" level.
  if ("glycan_structure" %in% colnames(glyexp::get_var_info(exp))) {
    aggregate(exp, to_level = "gfs")
  } else {
    aggregate(exp, to_level = "gf")
  }
}


.auto_batch_correct <- function(exp) {
  # Check if batch column exists
  if (!"batch" %in% colnames(exp$sample_info)) {
    return(exp)
  }

  # Determine if group column exists for use as covariate
  group_col <- if ("group" %in% colnames(exp$sample_info)) "group" else NULL
  
  # Detect batch effects
  p_values <- detect_batch_effect(exp, batch = "batch", group = group_col)
  
  # Calculate proportion of significant variables (p < 0.05)
  significant_vars <- sum(p_values < 0.05, na.rm = TRUE)
  total_vars <- length(p_values[!is.na(p_values)])
  prop_significant <- significant_vars / total_vars
  
  # Only correct batch effects if >10% of variables are significant
  if (prop_significant > 0.1) {
    cli::cli_progress_step("Correcting batch effects ({scales::percent(prop_significant, accuracy = 0.1)} of variables affected)")
    exp <- correct_batch_effect(exp, batch = "batch", group = group_col)
  } else {
    cli::cli_alert_info("Batch effects detected in {scales::percent(prop_significant, accuracy = 0.1)} of variables (<=10%). Skipping batch correction.")
  }
  
  exp
}
