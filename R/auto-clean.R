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
#' - [auto_normalize()]
#' - [normalize_total_area()]
#' - [auto_impute()]
#' - [auto_correct_batch_effect()]
#'
#' For glycoproteomics data, this function calls these functions in sequence:
#' - [auto_normalize()]
#' - [auto_remove()]
#' - [auto_impute()]
#' - [auto_aggregate()]
#' - [auto_normalize()]
#' - [auto_correct_batch_effect()]
#'
#' @param exp A [glyexp::experiment()] containing glycoproteomics or
#'   glycomics data.
#' @param group_col The column name in sample_info for groups. Default is "group".
#'   Can be NULL when no group information is available.
#' @param batch_col The column name in sample_info for batches. Default is "batch".
#'   Can be NULL when no batch information is available.
#' @param qc_name The name of QC samples in the `group_col` column. Default is "QC".
#'   Only used when `group_col` is not NULL. Can be NULL when no QC samples are available.
#' @param normalize_to_try Normalization functions to try. A list. Default includes:
#'   - [normalize_median()]: median normalization
#'   - [normalize_median_abs()]: absolute median normalization
#'   - [normalize_total_area()]: total area mormalization
#'   - [normalize_quantile()]: quantile normalization
#'   - [normalize_loessf()]: LoessF normalization
#'   - [normalize_loesscyc()]: LoessCyc normalization
#'   - [normalize_rlr()]: RLR normalization
#'   - [normalize_rlrma()]: RLRMA normalization
#'   - [normalize_rlrmacyc()]: RLRMAcyc normalization
#' @param impute_to_try Imputation functions to try. A list. Default includes:
#'   - [impute_zero()]: zero imputation
#'   - [impute_sample_min()]: sample-wise minimum imputation
#'   - [impute_half_sample_min()]: half sample-wise minimum imputation
#'   - [impute_sw_knn()]: sample-wise KNN imputation
#'   - [impute_fw_knn()]: feature-wise KNN imputation
#'   - [impute_bpca()]: BPCA imputation
#'   - [impute_ppca()]: PPCA imputation
#'   - [impute_svd()]: SVD imputation
#'   - [impute_min_prob()]: minimum probability imputation
#'   - [impute_miss_forest()]: MissForest imputation
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
#'
#' @return A modified `glyexp::experiment()` object.
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
  qc_name = "QC",
  normalize_to_try = NULL,
  impute_to_try = NULL,
  remove_preset = "discovery",
  batch_prop_threshold = 0.3,
  check_batch_confounding = TRUE,
  batch_confounding_threshold = 0.4
) {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(group_col, null.ok = TRUE)
  checkmate::assert_string(batch_col, null.ok = TRUE)
  checkmate::assert_string(qc_name, null.ok = TRUE)
  checkmate::assert_list(normalize_to_try, types = "function", null.ok = TRUE)
  checkmate::assert_list(impute_to_try, types = "function", null.ok = TRUE)
  checkmate::assert_choice(remove_preset, c("simple", "discovery", "biomarker"))
  checkmate::assert_number(batch_prop_threshold, lower = 0, upper = 1)
  checkmate::assert_flag(check_batch_confounding)
  checkmate::assert_number(batch_confounding_threshold, lower = 0, upper = 1)
  if (!checkmate::test_choice(glyexp::get_exp_type(exp), c("glycoproteomics", "glycomics"))) {
    cli::cli_abort(c(
      "The experiment type must be {.val glycoproteomics} or {.val glycomics}.",
      "x" = "Got {.val {glyexp::get_exp_type(exp)}}."
    ))
  }

  params <- list(
    group_col = group_col,
    qc_name = qc_name,
    normalize_to_try = normalize_to_try,
    impute_to_try = impute_to_try,
    remove_preset = remove_preset,
    batch_prop_threshold = batch_prop_threshold,
    check_batch_confounding = check_batch_confounding,
    batch_confounding_threshold = batch_confounding_threshold
  )
  info <- inspect_experiment(exp)
  switch(
    glyexp::get_exp_type(exp),
    glycoproteomics = .auto_clean_glycoproteomics(exp, params, info),
    glycomics = .auto_clean_glycomics(exp, params, info)
  )
}

.auto_clean_glycoproteomics <- function(exp, params, info) {
  cli::cli_h2("Normalizing data")
  exp <- auto_normalize(exp, params$group_col, params$qc_name, params$normalize_to_try, info)
  cli::cli_alert_success("Normalization completed.")
  cli::cli_h2("Removing variables with too many missing values")
  exp <- auto_remove(exp, params$remove_preset, params$group_col, params$qc_name, info)
  cli::cli_alert_success("Variable removal completed.")
  cli::cli_h2("Imputing missing values")
  exp <- auto_impute(exp, params$group_col, params$qc_name, params$impute_to_try, info)
  cli::cli_alert_success("Imputation completed.")
  cli::cli_h2("Aggregating data")
  exp <- auto_aggregate(exp)
  cli::cli_alert_success("Aggregation completed.")
  cli::cli_h2("Normalizing data again")
  exp <- auto_normalize(exp, params$group_col, params$qc_name, params$normalize_to_try, info)
  cli::cli_alert_success("Normalization completed.")
  cli::cli_h2("Correcting batch effects")
  exp <- auto_correct_batch_effect(
    exp,
    params$group_col,
    params$batch_col,
    params$batch_prop_threshold,
    params$check_batch_confounding,
    params$batch_confounding_threshold,
    info
  )
  cli::cli_alert_success("Batch correction completed.")
  exp
}

.auto_clean_glycomics <- function(exp, params, info) {
  cli::cli_h2("Removing variables with too many missing values")
  exp <- auto_remove(exp, params$remove_preset, params$group_col, params$qc_name, info)
  cli::cli_alert_success("Variable removal completed.")
  cli::cli_h2("Normalizing data")
  exp <- auto_normalize(exp, params$group_col, params$qc_name, params$normalize_to_try, info)
  cli::cli_alert_success("Normalization completed.")
  cli::cli_h2("Normalizing data (Total Area)")
  exp <- normalize_total_area(exp)
  cli::cli_alert_success("Total area normalization completed.")
  cli::cli_h2("Imputing missing values")
  exp <- auto_impute(exp, params$group_col, params$qc_name, params$impute_to_try, info)
  cli::cli_alert_success("Imputation completed.")
  cli::cli_h2("Correcting batch effects")
  exp <- auto_correct_batch_effect(
    exp,
    params$group_col,
    params$batch_col,
    params$batch_prop_threshold,
    params$check_batch_confounding,
    params$batch_confounding_threshold,
    info
  )
  cli::cli_alert_success("Batch correction completed.")
  exp
}
