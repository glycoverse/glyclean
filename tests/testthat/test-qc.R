skip_if_vdiffr <- function() {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("vdiffr")
}

test_that("plot_missing_heatmap works", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_heatmap", plot_missing_heatmap(test_exp))
})

test_that("plot_missing_heatmap handles no missing values", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  vdiffr::expect_doppelganger("plot_missing_heatmap_no_missing", plot_missing_heatmap(test_exp))
})

test_that("plot_missing_bar orders samples by missing proportion", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_bar_samples", plot_missing_bar(test_exp))
})

test_that("plot_missing_bar orders variables by missing proportion", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_bar_variables", plot_missing_bar(test_exp, on = "variables"))
})

test_that("plot_tic_bar orders samples by total intensity", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)

  vdiffr::expect_doppelganger("plot_tic_bar", plot_tic_bar(test_exp))
})

test_that("plot_rank_abundance orders proteins by mean log2 intensity", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)

  vdiffr::expect_doppelganger("plot_rank_abundance", plot_rank_abundance(test_exp))
})

test_that("plot_int_boxplot works", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)

  vdiffr::expect_doppelganger("plot_int_boxplot", plot_int_boxplot(test_exp))
})

test_that("plot_int_boxplot supports grouping", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  test_exp$sample_info$group <- c("A", "A", "B")

  vdiffr::expect_doppelganger(
    "plot_int_boxplot_by_group",
    plot_int_boxplot(test_exp, by = "group")
  )
})

test_that("plot_rle computes relative log expression values", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  vdiffr::expect_doppelganger("plot_rle", plot_rle(test_exp))
})

test_that("plot_rle supports grouping", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  test_exp$sample_info$group <- c("A", "A", "B")

  vdiffr::expect_doppelganger("plot_rle_by_group", plot_rle(test_exp, by = "group"))
})

test_that("plot_cv_dent computes CV per variable", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 3)
  vdiffr::expect_doppelganger("plot_cv_dent", plot_cv_dent(test_exp))
})

test_that("plot_cv_dent supports grouping", {
  skip_if_vdiffr()
  test_exp <- simple_exp(3, 4)
  test_exp$sample_info$group <- c("A", "A", "B", "B")
  vdiffr::expect_doppelganger("plot_cv_dent_by_group", plot_cv_dent(test_exp, by = "group"))
})

test_that("plot_batch_pca returns ggplot", {
  skip_if_vdiffr()
  skip_if_not_installed("factoextra")
  test_exp <- simple_exp(4, 4)
  test_exp$sample_info$batch <- c("A", "A", "B", "B")

  vdiffr::expect_doppelganger("plot_batch_pca", plot_batch_pca(test_exp, batch_col = "batch"))
})

test_that("plot_batch_pca rejects experiment without batch column", {
  skip_if_vdiffr()
  skip_if_not_installed("factoextra")
  test_exp <- simple_exp(4, 4)
  expect_error(plot_batch_pca(test_exp), "does not exist")
})

test_that("plot_rep_scatter draws replicate scatter plots", {
  skip_if_vdiffr()
  skip_if_not_installed("patchwork")
  test_exp <- simple_exp(5, 6)
  test_exp$sample_info$replicate <- rep(c("A", "B"), each = 3)

  withr::local_seed(123)
  vdiffr::expect_doppelganger(
    "plot_rep_scatter",
    plot_rep_scatter(test_exp, rep_col = "replicate", n_pairs = 2)
  )
})

test_that("plot_missing_heatmap returns ggplot or errors when dependencies missing", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA

  if (rlang::is_installed("pheatmap") && rlang::is_installed("ggplotify")) {
    plot <- plot_missing_heatmap(test_exp)
    expect_s3_class(plot, "ggplot")
  } else {
    expect_error(plot_missing_heatmap(test_exp), "pheatmap|ggplotify")
  }
})

test_that("plot_missing_bar validates on parameter and orders output", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  plot_samples <- plot_missing_bar(test_exp, on = "samples")
  expect_s3_class(plot_samples, "ggplot")
  expect_equal(plot_samples$labels$x, "Sample")
  expect_equal(levels(plot_samples$data$item), c("S3", "S1", "S2"))

  plot_vars <- plot_missing_bar(test_exp, on = "variables")
  expect_s3_class(plot_vars, "ggplot")
  expect_equal(plot_vars$labels$x, "Variable")
  expect_equal(levels(plot_vars$data$item), c("V3", "V2", "V1"))

  expect_error(plot_missing_bar(test_exp, on = "nope"), "should be one of")
})

test_that("plot_tic_bar orders samples by total intensity", {
  test_exp <- simple_exp(2, 3)
  plot <- plot_tic_bar(test_exp)
  expect_s3_class(plot, "ggplot")
  expect_equal(levels(plot$data$sample), c("S3", "S2", "S1"))
})

test_that("plot_rank_abundance falls back to variable names and errors on empty data", {
  test_exp <- simple_exp(3, 3)
  test_exp$var_info$protein <- c("P1", NA, "")
  plot <- plot_rank_abundance(test_exp)
  expect_s3_class(plot, "ggplot")
  expect_equal(as.character(plot$data$protein), c("V3", "V2", "P1"))

  empty_exp <- simple_exp(3, 3)
  empty_exp$expr_mat[,] <- 0
  expect_error(plot_rank_abundance(empty_exp), "No finite log2 intensity values")
})

test_that("plot_int_boxplot and plot_rle handle grouping inputs", {
  test_exp <- simple_exp(3, 3)
  test_exp$sample_info$group <- c("A", "A", "B")

  plot_int <- plot_int_boxplot(test_exp, by = "group")
  expect_s3_class(plot_int, "ggplot")

  plot_rle_obj <- plot_rle(test_exp, by = test_exp$sample_info$group)
  expect_s3_class(plot_rle_obj, "ggplot")

  expect_error(plot_rle(test_exp, by = c("A", "B")), "length 3")
})

test_that("plot_cv_dent errors when no finite CV values", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[,] <- 0
  expect_error(plot_cv_dent(test_exp), "No finite CV values")
})

test_that("plot_batch_pca validates inputs or requires package", {
  test_exp <- simple_exp(3, 3)
  test_exp$sample_info$batch <- c("A", "A", "B")

  if (rlang::is_installed("factoextra")) {
    plot <- plot_batch_pca(test_exp, batch_col = "batch")
    expect_s3_class(plot, "ggplot")
    expect_error(plot_batch_pca(test_exp, batch_col = "missing"), "does not exist")
  } else {
    expect_error(plot_batch_pca(test_exp, batch_col = "batch"), "factoextra")
  }
})

test_that("plot_rep_scatter validates rep_col and respects pair limits", {
  test_exp <- simple_exp(4, 4)
  test_exp$sample_info$replicate <- c("A", "A", "B", "B")

  expect_error(plot_rep_scatter(test_exp, rep_col = 123), "string")

  if (rlang::is_installed("patchwork")) {
    expect_message(
      plot_rep_scatter(test_exp, rep_col = "replicate", n_pairs = 10),
      "using all pairs"
    )
  } else {
    expect_error(plot_rep_scatter(test_exp, rep_col = "replicate", n_pairs = 2), "patchwork")
  }
})
