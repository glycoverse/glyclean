#' Automatic Data Preprocessing
#'
#' @description
#' Perform automatic data preprocessing on glycoproteomics or glycomics data.
#' This function applies an intelligent preprocessing pipeline that includes
#' normalization, missing value handling, imputation, aggregation (for
#' glycoproteomics data), and batch effect correction.
#'
#' For glycomics data, this function calls these functions in sequence:
#' - [auto_remove()]
#' - [auto_impute()]
#' - [auto_normalize()]
#' - [auto_correct_batch_effect()]
#'
#' For glycoproteomics data, this function calls these functions in sequence:
#' - [auto_remove()]
#' - [auto_normalize()]
#' - [auto_impute()]
#' - [auto_aggregate()]
#' - [auto_normalize()]
#' - [auto_correct_batch_effect()]
#'
#' @param exp A [glyexp::experiment()] containing glycoproteomics or glycomics
#'   data. [glyexp::GlycomicSE()] and [glyexp::GlycoproteomicSE()] objects are
#'   also supported.
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param batch_col The column name in sample_info for batches. Default is "batch".
#'   Can be NULL when no batch information is available.
#' @param remove_preset The preset for removing variables. Default is "discovery".
#'   Available presets:
#'   - "simple": remove variables with more than 50% missing values.
#'   - "discovery": more lenient, remove variables with more than 80% missing values,
#'     but ensure less than 50% of missing values in at least one group.
#'   - "biomarker": more strict, remove variables with more than 40% missing values,
#'     and ensure less than 60% of missing values in all groups.
#' @param batch_prop_threshold The proportion of variables that must show significant batch effects to perform batch correction.
#'   Default is 0.3 (30%).
#' @param check_batch_confounding Whether to check for confounding between batch and group variables.
#'   Default to TRUE.
#' @param batch_confounding_threshold The threshold for Cramer's V to consider batch and group variables highly confounded.
#'   Only used when `check_batch_confounding` is TRUE. Default to 0.4.
#' @param standardize_variable Whether to call [glyexp::standardize_variable()]
#'   after aggregation. Set to `FALSE` to skip network calls for faster testing.
#'   Default is `TRUE`.
#'
#' @return A modified [glyexp::experiment()] object. Glyco SE inputs return the
#'   same subclass.
#'
#' @examples
#' library(glyexp)
#' exp <- real_experiment
#' auto_clean(exp)
#'
#' @seealso [auto_normalize()], [auto_remove()], [auto_impute()], [auto_aggregate()], [auto_correct_batch_effect()]
#' @export
auto_clean <- function(
  exp,
  group_col = "group",
  batch_col = "batch",
  remove_preset = "discovery",
  batch_prop_threshold = 0.3,
  check_batch_confounding = TRUE,
  batch_confounding_threshold = 0.4,
  standardize_variable = TRUE
) {
  .assert_auto_container(exp)
  checkmate::assert_string(group_col, null.ok = TRUE)
  checkmate::assert_string(batch_col, null.ok = TRUE)
  checkmate::assert_choice(remove_preset, c("simple", "discovery", "biomarker"))
  checkmate::assert_number(batch_prop_threshold, lower = 0, upper = 1)
  checkmate::assert_flag(check_batch_confounding)
  checkmate::assert_number(batch_confounding_threshold, lower = 0, upper = 1)
  exp_type <- .get_exp_type(exp)
  if (!checkmate::test_choice(exp_type, c("glycoproteomics", "glycomics"))) {
    cli::cli_abort(c(
      "The experiment type must be {.val glycoproteomics} or {.val glycomics}.",
      "x" = "Got {.val {exp_type}}."
    ))
  }

  params <- list(
    group_col = group_col,
    batch_col = batch_col,
    remove_preset = remove_preset,
    batch_prop_threshold = batch_prop_threshold,
    check_batch_confounding = check_batch_confounding,
    batch_confounding_threshold = batch_confounding_threshold,
    standardize_variable = standardize_variable
  )
  switch(
    .get_exp_type(exp),
    glycoproteomics = .auto_clean_glycoproteomics(exp, params),
    glycomics = .auto_clean_glycomics(exp, params)
  )
}

.auto_clean_glycoproteomics <- function(exp, params) {
  cli::cli_h2("Removing variables with too many missing values")
  exp <- auto_remove(
    exp,
    preset = params$remove_preset,
    group_col = params$group_col
  )
  cli::cli_alert_success("Variable removal completed.")

  cli::cli_h2("Normalizing data")
  exp <- auto_normalize(
    exp,
    group_col = params$group_col
  )
  cli::cli_alert_success("Normalization completed.")

  cli::cli_h2("Imputing missing values")
  exp <- auto_impute(
    exp,
    params$group_col
  )
  cli::cli_alert_success("Imputation completed.")

  cli::cli_h2("Aggregating data")
  exp <- auto_aggregate(exp, standardize_variable = params$standardize_variable)
  cli::cli_alert_success("Aggregation completed.")

  cli::cli_h2("Normalizing data again")
  exp <- auto_normalize(
    exp,
    group_col = params$group_col
  )
  cli::cli_alert_success("Normalization completed.")

  cli::cli_h2("Correcting batch effects")
  exp <- auto_correct_batch_effect(
    exp,
    params$group_col,
    params$batch_col,
    params$batch_prop_threshold,
    params$check_batch_confounding,
    params$batch_confounding_threshold
  )
  cli::cli_alert_success("Batch correction completed.")
  exp
}

.auto_clean_glycomics <- function(exp, params) {
  cli::cli_h2("Removing variables with too many missing values")
  exp <- auto_remove(
    exp,
    preset = params$remove_preset,
    group_col = params$group_col
  )
  cli::cli_alert_success("Variable removal completed.")

  cli::cli_h2("Imputing missing values")
  exp <- auto_impute(
    exp,
    params$group_col
  )
  cli::cli_alert_success("Imputation completed.")

  cli::cli_h2("Normalizing data")
  exp <- auto_normalize(
    exp,
    group_col = params$group_col
  )
  cli::cli_alert_success("Normalization completed.")

  cli::cli_h2("Correcting batch effects")
  exp <- auto_correct_batch_effect(
    exp,
    params$group_col,
    params$batch_col,
    params$batch_prop_threshold,
    params$check_batch_confounding,
    params$batch_confounding_threshold
  )
  cli::cli_alert_success("Batch correction completed.")
  exp
}
