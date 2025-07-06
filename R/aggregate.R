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
#' - "gf": "protein", "gene", "glycan_composition", "protein_site"
#' - "gp": "peptide", "protein", "gene", "glycan_composition", "peptide_site", "protein_site"
#' - "gfs": "protein", "gene", "glycan_composition", "glycan_structure", "protein_site"
#' - "gps": "peptide", "protein", "gene", "glycan_composition", "glycan_structure",
#'          "peptide_site", "protein_site"
#'
#' Note that `aggregate()` will remove all other columns in the variable information
#' except the ones listed above.
#' So please call `aggregate()` as early as possible, 
#' before calling `add_site_seq()` or other motif annotation functions in `glymotif`.
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
#' @return A modified `glyexp_experiment` object
#'   with aggregated expression matrix and
#'   updated variable information.
#' @export
aggregate <- function(exp, to_level = c("gf", "gp", "gfs", "gps")) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  to_level <- rlang::arg_match(to_level)

  # Perform aggregation
  var_info_cols <- switch(
    to_level,
    gf = c("protein", "gene", "glycan_composition", "protein_site"),
    gp = c("peptide", "protein", "gene", "glycan_composition",
           "peptide_site", "protein_site"),
    gfs = c("protein", "gene", "glycan_composition", "glycan_structure",
            "protein_site"),
    gps = c("peptide", "protein", "gene", "glycan_composition",
            "glycan_structure", "peptide_site", "protein_site")
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
  var_info_df <- new_tb %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(var_info_cols))) %>%
    dplyr::mutate(variable = paste0("V", dplyr::row_number()), .before = 1)
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
