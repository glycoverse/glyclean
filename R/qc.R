#' Plot Missing Value Heatmap
#'
#' Draw a binary heatmap showing the missing value pattern in an experiment.
#' Values are binarized: 1 (present) or 0 (missing).
#' Rows (variables) are sorted by missing value proportion from low to high.
#' Columns (samples) are clustered using hierarchical clustering.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param ... Other arguments passed to `pheatmap::pheatmap()`.
#'
#' @returns A ggplot object of the missing value heatmap.
#'
#' @examples
#' plot_missing_heatmap(glyexp::toy_experiment)
#'
#' @export
plot_missing_heatmap <- function(exp, ...) {
  checkmate::assert_class(exp, "glyexp_experiment")
  rlang::check_installed("pheatmap")
  rlang::check_installed("ggplotify")

  mat <- exp$expr_mat

  # Binarize: 1 = present, 0 = missing
  binary_mat <- ifelse(is.na(mat), 0L, 1L)

  # Calculate missing proportion for each row and sort (low to high)
  missing_prop <- rowMeans(binary_mat == 0)
  row_order <- order(missing_prop)
  binary_mat <- binary_mat[row_order, , drop = FALSE]

  p <- pheatmap::pheatmap(
    binary_mat,
    cluster_rows = FALSE,
    cluster_cols = TRUE,
    clustering_distance_cols = "binary",
    clustering_method = "complete",
    color = c("grey80", "steelblue"),
    legend_breaks = c(0, 1),
    legend_labels = c("Missing", "Present"),
    show_rownames = FALSE,
    silent = TRUE,
    ...
  )

  ggplotify::as.ggplot(p)
}

#' Plot Missing Value Proportions by Sample or Variable
#'
#' Draw a bar plot of missing value proportions for each sample or variable.
#' Items are ordered from low to high missing proportion.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param on Whether to plot missingness by `"sample(s)"` or `"variable(s)"`.
#'   Defaults to `"sample"`.
#' @param ... Other arguments passed to `ggplot2::geom_col()`.
#'
#' @returns A ggplot object of missing value proportions by item.
#'
#' @examples
#' plot_missing_bar(glyexp::toy_experiment)
#'
#' @export
plot_missing_bar <- function(exp, on = "sample", ...) {
  checkmate::assert_class(exp, "glyexp_experiment")

  mat <- exp$expr_mat

  on <- match.arg(on, c("sample", "samples", "variable", "variables"))
  if (on %in% c("sample", "samples")) {
    item_names <- colnames(mat)
    if (is.null(item_names)) {
      item_names <- as.character(seq_len(ncol(mat)))
    }
    missing_prop <- colMeans(is.na(mat))
    x_label <- "Sample"
  } else {
    item_names <- rownames(mat)
    if (is.null(item_names)) {
      item_names <- as.character(seq_len(nrow(mat)))
    }
    missing_prop <- rowMeans(is.na(mat))
    x_label <- "Variable"
  }

  names(missing_prop) <- item_names
  item_order <- item_names[order(missing_prop)]

  plot_data <- tibble::tibble(
    item = factor(item_order, levels = item_order),
    missing_prop = missing_prop[item_order]
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = item, y = missing_prop)) +
    ggplot2::geom_col(...) +
    ggplot2::labs(x = x_label, y = "Missing proportion")
}
