#' Aggregate Data
#'
#' Aggregate glycoproteomics data to different levels
#' (glycoforms, glycopeptides, etc.).
#' This function sums up quantitative values for each
#' unique combination of specified variables.
#' It is recommended to call this function after missing value imputation.
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
#' If `aggregate` has never been called on an experiment,
#' it has a "psm" (peptide-spectrum match) level by default.
#' You can aggregate an experiment from "psm" to all other levels.
#' You can also call `aggregate` on an experiment that has already been aggregated.
#' However, you cannot aggregate from "gf" to "gp",
#' as the peptide information is lost during "gf" aggregation.
#' Similarly, you cannot aggregate from "gf" to "gfs", or "gp" to "gps".
#'
#' @return A modified `glyexp_experiment` object
#'   with aggregated expression matrix and
#'   updated variable information.
#' @export
aggregate <- function(x, to_level = c("gf", "gp", "gfs", "gps")) {
  # Check arguments
  checkmate::assert_class(x, "glyexp_experiment")
  to_level <- rlang::arg_match(to_level)

  # Check if the conversion is valid
  current_level <- ifelse(
    is.null(x$meta_data$aggr_level),
    "psm",
    x$meta_data$aggr_level
  )
  invalid_conversions <- list(
    c("gf", "gp"),
    c("gf", "gfs"),
    c("gp", "gps"),
    c("gfs", "gps"),
    c("gfs", "gp")
  )
  if (purrr::some(invalid_conversions, ~ all(.x == c(current_level, to_level)))) {
    cli::cli_abort(c(
      "Cannot aggregate from {.val {current_level}} to {.val {to_level}}.",
      i = "See {.run ?aggregate} for more details."
    ))
  }

  # Perform aggregation
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
  sample_info_df <- x$sample_info
  tb <- tibble::as_tibble(x, sample_cols = NULL)
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
  new_exp <- x
  new_exp$expr_mat <- expr_mat
  new_exp$var_info <- var_info_df
  new_exp$meta_data$aggr_level <- to_level
  new_exp
}
