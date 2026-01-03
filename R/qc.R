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
    breaks = c(-0.5, 0.5, 1.5),
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
#'
#' @returns A ggplot object of missing value proportions by item.
#'
#' @examples
#' plot_missing_bar(glyexp::toy_experiment)
#'
#' @export
plot_missing_bar <- function(exp, on = "sample") {
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

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$item, y = .data$missing_prop)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = x_label, y = "Missing proportion")
}

#' Plot Total Intensity by Sample
#'
#' Draw a bar plot showing total intensity (TIC) for each sample. Samples are
#' ordered from high to low TIC from left to right.
#'
#' @param exp A [glyexp::experiment()] object.
#'
#' @returns A ggplot object of total intensity by sample.
#'
#' @examples
#' plot_tic_bar(glyexp::toy_experiment)
#'
#' @export
plot_tic_bar <- function(exp) {
  checkmate::assert_class(exp, "glyexp_experiment")

  mat <- exp$expr_mat
  sample_names <- colnames(mat)
  if (is.null(sample_names)) {
    sample_names <- as.character(seq_len(ncol(mat)))
  }

  tic <- colSums(mat, na.rm = TRUE)
  names(tic) <- sample_names

  sample_order <- sample_names[order(tic, decreasing = TRUE)]

  plot_data <- tibble::tibble(
    sample = factor(sample_order, levels = sample_order),
    tic = tic[sample_order]
  )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$sample, y = .data$tic)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Sample", y = "Total intensity")
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
#'
#' @returns A ggplot object of log-intensity boxplots.
#'
#' @examples
#' plot_int_boxplot(glyexp::toy_experiment)
#' plot_int_boxplot(glyexp::toy_experiment, by = "group")
#'
#' @export
plot_int_boxplot <- function(exp, by = NULL) {
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

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$sample, y = .data$log_intensity))
  if (!is.null(by_values)) {
    plot <- plot + ggplot2::geom_boxplot(ggplot2::aes(fill = .data$group))
  } else {
    plot <- plot + ggplot2::geom_boxplot()
  }

  plot + ggplot2::labs(x = "Sample", y = "Log2 intensity")
}

#' Plot Relative Log Expression (RLE) Boxplots
#'
#' Draw boxplots of relative log expression (log2 intensity minus row median)
#' for each sample. Optionally color and group samples by a metadata variable.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param by Grouping variable for samples. Can be a column name in `sample_info`
#'   or a vector/factor with length equal to the number of samples. When provided,
#'   samples are grouped along the x-axis and boxplots are colored by group.
#'
#' @returns A ggplot object of RLE boxplots.
#'
#' @examples
#' plot_rle(glyexp::toy_experiment)
#' plot_rle(glyexp::toy_experiment, by = "group")
#'
#' @export
plot_rle <- function(exp, by = NULL) {
  checkmate::assert_class(exp, "glyexp_experiment")

  mat <- exp$expr_mat
  sample_names <- colnames(mat)
  if (is.null(sample_names)) {
    sample_names <- as.character(seq_len(ncol(mat)))
  }

  log_mat <- log2(mat)
  log_mat[!is.finite(log_mat)] <- NA_real_
  row_medians <- matrixStats::rowMedians(log_mat, na.rm = TRUE)
  rle_mat <- sweep(log_mat, 1, row_medians, "-")
  colnames(rle_mat) <- sample_names

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

  plot_data <- tibble::as_tibble(rle_mat, .name_repair = "minimal")
  plot_data <- tidyr::pivot_longer(
    plot_data,
    cols = dplyr::all_of(sample_names),
    names_to = "sample",
    values_to = "rle"
  )
  if (!is.null(by_values)) {
    plot_data <- dplyr::left_join(plot_data, group_df, by = "sample")
  }

  plot_data$sample <- factor(plot_data$sample, levels = sample_order)

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = sample, y = rle))
  if (!is.null(by_values)) {
    plot <- plot + ggplot2::geom_boxplot(ggplot2::aes(fill = .data$group))
  } else {
    plot <- plot + ggplot2::geom_boxplot()
  }

  plot + ggplot2::labs(x = "Sample", y = "Relative log expression")
}

#' Plot CV Density
#'
#' Compute coefficient of variation (CV) for each variable and plot its density.
#' When `by` is provided, CVs are computed within each group and densities are
#' shown with different fills.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param by Grouping variable for samples. Can be a column name in `sample_info`
#'   or a vector/factor with length equal to the number of samples. When provided,
#'   CVs are computed within each group and densities are shown with different fills.
#'
#' @returns A ggplot object of CV density.
#'
#' @examples
#' plot_cv_dent(glyexp::toy_experiment)
#' exp <- glyexp::toy_experiment
#' exp$sample_info$group <- rep(c("A", "B"), length.out = ncol(exp$expr_mat))
#' plot_cv_dent(exp, by = "group")
#'
#' @export
plot_cv_dent <- function(exp, by = NULL) {
  checkmate::assert_class(exp, "glyexp_experiment")

  mat <- exp$expr_mat
  var_names <- rownames(mat)
  if (is.null(var_names)) {
    var_names <- as.character(seq_len(nrow(mat)))
  }

  by_values <- .resolve_column_param(
    by,
    sample_info = exp$sample_info,
    param_name = "by",
    n_samples = ncol(mat),
    allow_null = TRUE
  )

  if (is.null(by_values)) {
    cvs <- apply(mat, 1, .cv)
    plot_data <- tibble::tibble(variable = var_names, cv = cvs)
  } else {
    cv_mat <- .summarize_vars_mat(mat, .cv, by = by_values)
    rownames(cv_mat) <- var_names
    plot_data <- tibble::as_tibble(cv_mat, rownames = "variable")
    plot_data <- tidyr::pivot_longer(
      plot_data,
      cols = -dplyr::all_of("variable"),
      names_to = "group",
      values_to = "cv"
    )
    plot_data$group <- factor(plot_data$group, levels = colnames(cv_mat))
  }

  plot_data <- dplyr::filter(plot_data, is.finite(.data$cv))
  if (nrow(plot_data) == 0) {
    cli::cli_abort("No finite CV values available for plotting.")
  }

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$cv))
  if (is.null(by_values)) {
    plot <- plot + ggplot2::geom_density()
  } else {
    plot <- plot + ggplot2::geom_density(ggplot2::aes(fill = .data$group), alpha = 0.4)
  }

  plot <- plot + ggplot2::labs(x = "CV", y = "Density")
  if (!is.null(by_values)) {
    plot <- plot + ggplot2::labs(fill = "Group")
  }

  plot
}

#' Plot PCA Score by Batch
#'
#' Draw a PCA score plot for samples and color points by batch.
#' PCA is computed on log2-transformed intensities after removing variables
#' with missing values.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param batch_col Column name in `sample_info`, or a factor/vector with length
#'   equal to the number of samples.
#'
#' @returns A ggplot object of PCA scores.
#'
#' @examples
#' exp <- glyexp::toy_experiment
#' exp$sample_info$batch <- rep(c("A", "B"), each = 3)
#' plot_batch_pca(exp, batch_col = "batch")
#'
#' @export
plot_batch_pca <- function(exp, batch_col = "batch") {
  checkmate::assert_class(exp, "glyexp_experiment")
  rlang::check_installed("factoextra", reason = "to use `plot_batch_pca()`")

  mat <- exp$expr_mat
  if (ncol(mat) < 2 || nrow(mat) < 2) {
    cli::cli_abort("PCA requires at least two samples and two variables.")
  }

  sample_names <- colnames(mat)
  if (is.null(sample_names)) {
    sample_names <- as.character(seq_len(ncol(mat)))
  }

  batch_values <- .resolve_column_param(
    batch_col,
    sample_info = exp$sample_info,
    param_name = "batch_col",
    n_samples = ncol(mat),
    allow_null = FALSE
  )
  batch_factor <- if (is.factor(batch_values)) {
    droplevels(batch_values)
  } else {
    factor(as.character(batch_values))
  }

  log_mat <- log2(mat)
  log_mat[!is.finite(log_mat)] <- NA_real_
  complete_rows <- stats::complete.cases(log_mat)
  if (!any(complete_rows)) {
    cli::cli_abort("No complete variables available for PCA after removing missing values.")
  }
  log_mat <- log_mat[complete_rows, , drop = FALSE]

  row_sds <- matrixStats::rowSds(log_mat)
  log_mat <- log_mat[row_sds > 0, , drop = FALSE]
  if (nrow(log_mat) < 2) {
    cli::cli_abort("PCA requires at least two variables with non-zero variance.")
  }

  pca_mat <- t(log_mat)
  rownames(pca_mat) <- sample_names
  if (min(dim(pca_mat)) < 2) {
    cli::cli_abort("PCA requires at least two samples and two variables after filtering.")
  }

  pca <- stats::prcomp(pca_mat, center = TRUE, scale. = TRUE)

  factoextra::fviz_pca_ind(
    pca,
    geom.ind = "point",
    habillage = batch_factor
  ) +
    ggplot2::labs(color = "Batch")
}

#' Plot Replicate Scatter Plots
#'
#' Randomly draw replicate sample pairs and plot log2 intensity scatter plots.
#' The plot title shows sample names, and the subtitle reports the R2 value.
#'
#' @param exp A [glyexp::experiment()] object.
#' @param rep_col Column name in `sample_info` used to define replicate groups.
#'   Samples with the same value in this column are treated as replicates
#'   (e.g. `c("A", "A", "A", "B", "B", "B")` indicates three replicates for
#'   sample A and three for sample B).
#' @param n_pairs Number of replicate pairs to draw at random.
#'
#' @returns A patchwork object containing replicate scatter plots.
#'
#' @examples
#' exp <- glyexp::toy_experiment
#' exp$sample_info$replicate <- rep(c("A", "B"), length.out = ncol(exp$expr_mat))
#' plot_rep_scatter(exp, rep_col = "replicate", n_pairs = 4)
#'
#' @export
plot_rep_scatter <- function(exp, rep_col, n_pairs = 9) {
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(rep_col)
  checkmate::assert_int(n_pairs, lower = 1)
  rlang::check_installed("patchwork", reason = "to use `plot_rep_scatter()`")

  mat <- exp$expr_mat
  if (ncol(mat) < 2 || nrow(mat) < 2) {
    cli::cli_abort("Replicate scatter plots require at least two samples and two variables.")
  }

  sample_names <- colnames(mat)
  if (is.null(sample_names)) {
    sample_names <- as.character(seq_len(ncol(mat)))
  }

  rep_values <- .resolve_column_param(
    rep_col,
    sample_info = exp$sample_info,
    param_name = "rep_col",
    n_samples = ncol(mat),
    allow_null = FALSE
  )
  if (anyNA(rep_values)) {
    cli::cli_abort("The {.arg rep_col} column contains missing values.")
  }

  rep_levels <- if (is.factor(rep_values)) {
    levels(droplevels(rep_values))
  } else {
    unique(as.character(rep_values))
  }
  rep_factor <- factor(as.character(rep_values), levels = rep_levels)
  group_samples <- split(sample_names, rep_factor)

  pair_list <- purrr::map(group_samples, function(samples) {
    if (length(samples) < 2) {
      return(list())
    }
    combn(samples, 2, simplify = FALSE)
  })
  pairs <- purrr::flatten(pair_list)
  if (length(pairs) == 0) {
    cli::cli_abort("No replicate pairs available from {.arg rep_col}.")
  }

  if (n_pairs > length(pairs)) {
    cli::cli_alert_warning(
      "Requested {n_pairs} pairs but only {length(pairs)} available; using all pairs."
    )
    n_pairs <- length(pairs)
  }

  selected_pairs <- pairs[sample.int(length(pairs), size = n_pairs)]

  log_mat <- log2(mat)
  log_mat[!is.finite(log_mat)] <- NA_real_
  colnames(log_mat) <- sample_names

  plots <- purrr::map(selected_pairs, function(pair) {
    plot_data <- tibble::tibble(
      x = log_mat[, pair[1]],
      y = log_mat[, pair[2]]
    )
    plot_data <- dplyr::filter(plot_data, is.finite(.data$x), is.finite(.data$y))

    if (nrow(plot_data) < 2) {
      r2 <- NA_real_
    } else {
      r_val <- stats::cor(plot_data$x, plot_data$y, use = "complete.obs")
      r2 <- r_val^2
    }

    subtitle <- if (is.na(r2)) {
      "R2: NA"
    } else {
      sprintf("R2: %.3f", r2)
    }

    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::labs(
        x = paste0(pair[1], " log2 intensity"),
        y = paste0(pair[2], " log2 intensity"),
        title = paste(pair[1], "vs", pair[2]),
        subtitle = subtitle
      )
  })

  patchwork::wrap_plots(plots)
}
