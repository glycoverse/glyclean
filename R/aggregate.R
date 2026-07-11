#' Aggregate Data
#'
#' @description
#' Aggregate glycoproteomics data to different levels
#' (glycoforms, glycopeptides, etc.).
#' This function sums up quantitative values for each
#' unique combination of specified variables.
#' It is recommended to call this function after missing value imputation.
#'
#' The following levels are available:
#' - "gf": Aggregate to glycoforms, which is the unique combination of proteins,
#'   protein sites, and glycan compositions.
#'   This is the default level.
#' - "gp": Aggregate to glycopeptides,
#'   which is the unique combination of peptides,
#'   proteins, protein sites, and glycan compositions.
#' - "gfs": Like "gf", but differentiates structures with the same composition.
#' - "gps": Like "gp", but differentiates structures with the same composition.
#'
#' Different levels of aggregation require different columns in the variable information.
#' - "gf": "protein", "glycan_composition", "protein_site"
#' - "gp": "peptide", "protein", "glycan_composition", "peptide_site", "protein_site"
#' - "gfs": "protein", "glycan_composition", "glycan_structure", "protein_site"
#' - "gps": "peptide", "protein", "glycan_composition", "glycan_structure",
#'          "peptide_site", "protein_site"
#'
#' Other columns in the variable information tibble with "many-to-one" relationship
#' with the unique combination of columns listed above will be kept.
#' A common example is the "gene" column.
#' For each "glycoform" (unique combination of "protein", "protein_site", and "glycan_composition"),
#' there should be only one "gene" value, therefore it is kept for "gf" level.
#' On the other hand, the "peptide" column is removed for "gf" level,
#' as one "glycoform" can contain multiple "peptides".
#'
#' @param exp A [glyexp::experiment()] object with "glycoproteomics" type.
#'   [glyexp::GlycoproteomicSE()] objects are also supported.
#' @param to_level The aggregation level,
#'   one of: "gf" (glycoforms), "gp" (glycopeptides),
#'   "gfs" (glycoforms with structures),
#'   or "gps" (glycopeptides with structures).
#'   See Details for more information.
#' @param standardize_variable Whether to call [glyexp::standardize_variable()]
#'   after aggregation. Set to `FALSE` to skip network calls for faster testing.
#'   Default is `TRUE`.
#' @returns A modified [glyexp::experiment()] object with an aggregated
#'   expression matrix and updated variable information. A
#'   [glyexp::GlycoproteomicSE()] input returns the same subclass.
#' @export
aggregate <- function(
  exp,
  to_level = c("gf", "gp", "gfs", "gps"),
  standardize_variable = TRUE
) {
  glyclean_aggregate(
    exp,
    to_level = to_level,
    standardize_variable = standardize_variable
  )
}

glyclean_aggregate <- function(
  exp,
  to_level = c("gf", "gp", "gfs", "gps"),
  standardize_variable = TRUE
) {
  UseMethod("glyclean_aggregate")
}

#' @rdname aggregate
#' @export
glyclean_aggregate.glyexp_experiment <- function(
  exp,
  to_level = c("gf", "gp", "gfs", "gps"),
  standardize_variable = TRUE
) {
  .aggregate_container(
    exp,
    to_level = to_level,
    standardize_variable = standardize_variable,
    error_call = quote(glyclean_aggregate())
  )
}

#' @export
#' @noRd
glyclean_aggregate.GlycomicSE <- function(
  exp,
  to_level = c("gf", "gp", "gfs", "gps"),
  standardize_variable = TRUE
) {
  .aggregate_container(
    exp,
    to_level = to_level,
    standardize_variable = standardize_variable,
    error_call = quote(glyclean_aggregate())
  )
}

#' @export
#' @noRd
glyclean_aggregate.GlycoproteomicSE <- function(
  exp,
  to_level = c("gf", "gp", "gfs", "gps"),
  standardize_variable = TRUE
) {
  .aggregate_container(
    exp,
    to_level = to_level,
    standardize_variable = standardize_variable,
    error_call = quote(glyclean_aggregate())
  )
}

#' Aggregate a supported glycoproteomics container
#'
#' @inheritParams aggregate
#' @param error_call The call to use in error messages.
#'
#' @return A container with the same class as `exp`.
#' @noRd
.aggregate_container <- function(
  exp,
  to_level = c("gf", "gp", "gfs", "gps"),
  standardize_variable = TRUE,
  error_call = rlang::caller_call()
) {
  # Check arguments
  .assert_glycoproteomics_container(exp, error_call = error_call)
  to_level <- rlang::arg_match(to_level)

  # Perform aggregation
  var_info_cols <- switch(
    to_level,
    gf = c("protein", "glycan_composition", "protein_site"),
    gp = c(
      "peptide",
      "protein",
      "glycan_composition",
      "peptide_site",
      "protein_site"
    ),
    gfs = c(
      "protein",
      "glycan_composition",
      "glycan_structure",
      "protein_site"
    ),
    gps = c(
      "peptide",
      "protein",
      "glycan_composition",
      "glycan_structure",
      "peptide_site",
      "protein_site"
    )
  )
  var_info <- .get_var_info(exp)
  missing_cols <- setdiff(var_info_cols, colnames(var_info))
  if (length(missing_cols) > 0) {
    if (length(missing_cols) == 1 && missing_cols == "glycan_structure") {
      # special case for missing "glycan_structure" column
      cli::cli_abort(
        c(
          "All required columns must be present in `var_info`.",
          "i" = "Required columns: {.field {var_info_cols}}.",
          "x" = "Missing columns: {.field {missing_cols}}.",
          "i" = "You might want to aggregate to {.val gp} or {.val gf} level."
        ),
        call = error_call
      )
    }
    cli::cli_abort(
      c(
        "All required columns must be present in `var_info`.",
        "i" = "Required columns: {.field {var_info_cols}}.",
        "x" = "Missing columns: {.field {missing_cols}}."
      ),
      call = error_call
    )
  }
  group_id <- .get_aggregation_group_id(var_info, var_info_cols)
  var_info_df <- .get_aggr_var_info(var_info, var_info_cols, group_id) |>
    dplyr::mutate(variable = paste0("V", dplyr::row_number()), .before = 1)
  expr_mat <- rowsum(
    .get_expr_mat(exp),
    group = group_id,
    reorder = FALSE,
    na.rm = TRUE
  )
  rownames(expr_mat) <- var_info_df$variable
  sample_info_df <- .get_sample_info(exp)
  expr_mat <- expr_mat[
    var_info_df$variable,
    sample_info_df$sample,
    drop = FALSE
  ]
  new_exp <- .rebuild_container(
    exp,
    expr_mat = expr_mat,
    var_info = var_info_df
  )
  if (standardize_variable) {
    suppressMessages(.standardize_container_variable(new_exp))
  } else {
    new_exp
  }
}

#' Get aggregation group identifiers
#'
#' Create integer group identifiers in the order that combinations of
#' aggregation columns first occur in the variable information.
#'
#' @param var_info A tibble with the variable information.
#' @param aggr_cols Column names used for aggregation.
#'
#' @returns An integer vector with one group identifier per variable.
#'
#' @noRd
.get_aggregation_group_id <- function(var_info, aggr_cols) {
  group_id <- var_info |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(aggr_cols))) |>
    dplyr::group_indices()
  match(group_id, unique(group_id))
}

#' @rdname aggregate
#' @export
glyclean_aggregate.default <- function(
  exp,
  to_level = c("gf", "gp", "gfs", "gps"),
  standardize_variable = TRUE
) {
  cli::cli_abort(c(
    "{.arg exp} must be a {.cls glyexp_experiment} object or a {.cls GlycoproteomicSE} object.",
    "x" = "Got {.cls {class(exp)}}."
  ))
}

#' Get Aggregation Descriptive Columns
#'
#' Get all columns in the variable information tibble that have "many-to-one" relationship
#' with the unique combination of given columns.
#'
#' @param var_info A tibble with the variable information.
#' @param aggr_cols Column names used for aggregation.
#' @param group_id Integer aggregation group identifiers in first-occurrence
#'   order.
#'
#' @returns A tibble with the distinct values of the descriptive columns.
#'
#' @noRd
.get_aggr_var_info <- function(var_info, aggr_cols, group_id) {
  descriptive_cols <- setdiff(colnames(var_info), c("variable", aggr_cols))
  group_col <- make.unique(c(colnames(var_info), ".aggregate_group"))[[
    ncol(var_info) + 1L
  ]]
  distinct_counts <- var_info |>
    dplyr::mutate(!!group_col := .env$group_id) |>
    dplyr::summarise(
      dplyr::across(tidyselect::all_of(descriptive_cols), dplyr::n_distinct),
      .by = tidyselect::all_of(group_col)
    )
  kept_cols <- purrr::keep(
    descriptive_cols,
    ~ all(distinct_counts[[.x]] == 1L)
  )
  var_info |>
    dplyr::filter(!duplicated(.env$group_id)) |>
    dplyr::select(tidyselect::all_of(c(aggr_cols, kept_cols)))
}
