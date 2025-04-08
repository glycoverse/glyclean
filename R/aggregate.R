#' Aggregate Data
#'
#' Aggregate glycoproteomics data to different levels
#' (glycoforms, glycopeptides, etc.).
#' This function sums up quantitative values for each
#' unique combination of specified variables.
#'
#' @param exp A `glyexp_experiment` object containing glycoproteomics data.
#' @param to_level The aggregation level,
#'   one of: "gf" (glycoforms), "gp" (glycopeptides),
#'   "gfs" (glycoforms with structures),
#'   or "gps" (glycopeptides with structures).
#'   See Details for more information.
#'
#' @details
#' The `to_level` parameter determines the level of aggregation.
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
#' It is recommended to call this function after missing value imputation.
#'
#' @return A modified `glyexp_experiment` object
#'   with aggregated expression matrix and
#'   updated variable information.
#' @export
aggregate <- function(exp, to_level = c("gf", "gp", "gfs", "gps")) {
  checkmate::assert_class(exp, "glyexp_experiment")
  to_level <- rlang::arg_match(to_level)
  var_info_cols <- switch(
    to_level,
    gf = c("proteins", "genes", "glycan_composition", "protein_sites"),
    gp = c("peptide", "proteins", "genes", "glycan_composition",
           "peptide_site", "protein_sites"),
    gfs = c("proteins", "genes", "glycan_composition", "glycan_structure",
            "protein_sites"),
    gps = c("peptide", "proteins", "genes", "glycan_composition",
            "glycan_structure", "peptide_site", "protein_sites")
  )
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
