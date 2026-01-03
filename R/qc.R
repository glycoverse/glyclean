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

#' Plot Log-Intensity Boxplots by Sample
#'
#' Draw boxplots of log2-transformed intensities for each sample.
#' Optionally color and group samples by a metadata variable.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param by Grouping variable for samples. Can be a column name in `sample_info`
#'   or a vector/factor with length equal to the number of samples. When provided,
#'   samples are grouped along the x-axis and boxplots are colored by group.
#' @param ... Other arguments passed to `ggplot2::geom_boxplot()`.
#'
#' @returns A ggplot object of log-intensity boxplots.
#'
#' @examples
#' plot_int_boxplot(glyexp::toy_experiment)
#' plot_int_boxplot(glyexp::toy_experiment, by = "group")
#'
#' @export
plot_int_boxplot <- function(exp, by = NULL, ...) {
  checkmate::assert_class(exp, "glyexp_experiment")

  mat <- exp$expr_mat
  sample_names <- colnames(mat)
  if (is.null(sample_names)) {
    sample_names <- as.character(seq_len(ncol(mat)))
  }

  log_mat <- log2(mat)
  log_mat[!is.finite(log_mat)] <- NA_real_
  colnames(log_mat) <- sample_names

  by_values <- .resolve_column_param(
    by,
    sample_info = exp$sample_info,
    param_name = "by",
    n_samples = ncol(mat),
    allow_null = TRUE
  )

  sample_order <- sample_names
  if (!is.null(by_values)) {
    group_levels <- if (is.factor(by_values)) levels(by_values) else unique(as.character(by_values))
    group_factor <- factor(as.character(by_values), levels = group_levels)
    group_df <- tibble::tibble(sample = sample_names, group = group_factor)
    sample_order <- sample_names[order(match(group_factor, group_levels))]
  }

  plot_data <- tibble::as_tibble(log_mat, .name_repair = "minimal")
  plot_data <- tidyr::pivot_longer(
    plot_data,
    cols = dplyr::all_of(sample_names),
    names_to = "sample",
    values_to = "log_intensity"
  )
  if (!is.null(by_values)) {
    plot_data <- dplyr::left_join(plot_data, group_df, by = "sample")
  }

  plot_data$sample <- factor(plot_data$sample, levels = sample_order)

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sample, y = log_intensity))
  if (!is.null(by_values)) {
    plot <- plot + ggplot2::geom_boxplot(ggplot2::aes(fill = group), ...)
  } else {
    plot <- plot + ggplot2::geom_boxplot(...)
  }

  plot + ggplot2::labs(x = "Sample", y = "Log2 intensity")
}
