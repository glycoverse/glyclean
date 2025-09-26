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
#' @param exp A `glyexp_experiment` object containing glycoproteomics data.
#'   This function only works with `glyexp_experiment` objects as it requires
#'   variable information for aggregation.
#' @param to_level The aggregation level,
#'   one of: "gf" (glycoforms), "gp" (glycopeptides),
#'   "gfs" (glycoforms with structures),
#'   or "gps" (glycopeptides with structures).
#'   See Details for more information.
#'
#' @returns A modified `glyexp_experiment` object with aggregated expression matrix and
#'   updated variable information.
#' @export
aggregate <- function(exp, to_level = c("gf", "gp", "gfs", "gps")) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  to_level <- rlang::arg_match(to_level)

  # Perform aggregation
  var_info_cols <- switch(
    to_level,
    gf = c("protein", "glycan_composition", "protein_site"),
    gp = c("peptide", "protein", "glycan_composition", "peptide_site", "protein_site"),
    gfs = c("protein", "glycan_composition", "glycan_structure", "protein_site"),
    gps = c("peptide", "protein", "glycan_composition", "glycan_structure", "peptide_site", "protein_site")
  )
  var_info <- exp$var_info
  missing_cols <- setdiff(var_info_cols, colnames(var_info))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing columns in variable information: {.field {missing_cols}}.",
      i = "See {.run ?aggregate} for more details."
    ))
  }
  sample_info_df <- exp$sample_info
  tb <- tibble::as_tibble(exp, sample_cols = NULL)
  new_tb <- dplyr::summarise(
    tb,
    value = sum(.data$value, na.rm = TRUE),
    .by = tidyselect::all_of(c(var_info_cols, "sample")),
  )
  add_cols <- .get_aggr_var_info(var_info, var_info_cols)
  var_info_df <- new_tb %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(var_info_cols))) %>%
    dplyr::mutate(variable = paste0("V", dplyr::row_number()), .before = 1) %>%
    dplyr::left_join(add_cols, by = var_info_cols)
  expr_mat <- new_tb %>%
    dplyr::left_join(var_info_df, by = var_info_cols) %>%
    dplyr::select(tidyselect::all_of(c("sample", "variable", "value"))) %>%
    tidyr::pivot_wider(names_from = "sample", values_from = "value") %>%
    tibble::column_to_rownames("variable") %>%
    as.matrix()
  expr_mat <- expr_mat[
    var_info_df$variable,
    sample_info_df$sample,
    drop = FALSE
  ]
  new_exp <- exp
  new_exp$expr_mat <- expr_mat
  new_exp$var_info <- var_info_df
  new_exp
}

#' Get Aggregation Descriptive Columns
#'
#' Get all columns in the variable information tibble that have "many-to-one" relationship
#' with the unique combination of given columns.
#'
#' @param var_info A tibble with the variable information.
#' @param aggr_cols Column names used for aggregation.
#'
#' @returns A tibble with the distinct values of the descriptive columns.
#'
#' @noRd
.get_aggr_var_info <- function(var_info, aggr_cols) {
  cols <- var_info |>
    dplyr::select(-dplyr::all_of("variable")) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), dplyr::n_distinct),
      .by = dplyr::all_of(aggr_cols)
    ) |>
    dplyr::select(-dplyr::all_of(aggr_cols)) |>
    dplyr::select(dplyr::where(~ all(.x == 1))) |>
    colnames()
  var_info |>
    dplyr::select(dplyr::all_of(c(aggr_cols, cols))) |>
    dplyr::distinct()
}