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
