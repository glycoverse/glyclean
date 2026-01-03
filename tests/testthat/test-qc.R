skip_on_ci()
skip_on_cran()

test_that("plot_missing_heatmap works", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_heatmap", plot_missing_heatmap(test_exp))
})

test_that("plot_missing_heatmap handles no missing values", {
  test_exp <- simple_exp(3, 3)
  vdiffr::expect_doppelganger("plot_missing_heatmap_no_missing", plot_missing_heatmap(test_exp))
})

test_that("plot_missing_bar orders samples by missing proportion", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_bar_samples", plot_missing_bar(test_exp))
})

test_that("plot_missing_bar orders variables by missing proportion", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_bar_variables", plot_missing_bar(test_exp, on = "variables"))
})

test_that("plot_tic_bar orders samples by total intensity", {
  test_exp <- simple_exp(3, 3)

  vdiffr::expect_doppelganger("plot_tic_bar", plot_tic_bar(test_exp))
})

test_that("plot_int_boxplot works", {
  test_exp <- simple_exp(3, 3)

  vdiffr::expect_doppelganger("plot_int_boxplot", plot_int_boxplot(test_exp))
})

test_that("plot_int_boxplot supports grouping", {
  test_exp <- simple_exp(3, 3)
  test_exp$sample_info$group <- c("A", "A", "B")

  vdiffr::expect_doppelganger(
    "plot_int_boxplot_by_group",
    plot_int_boxplot(test_exp, by = "group")
  )
})

test_that("plot_rle computes relative log expression values", {
  test_exp <- simple_exp(3, 3)
  vdiffr::expect_doppelganger("plot_rle", plot_rle(test_exp))
})

test_that("plot_rle supports grouping", {
  test_exp <- simple_exp(3, 3)
  test_exp$sample_info$group <- c("A", "A", "B")

  vdiffr::expect_doppelganger("plot_rle_by_group", plot_rle(test_exp, by = "group"))
})

test_that("plot_cv_dent computes CV per variable", {
  test_exp <- simple_exp(3, 3)
  vdiffr::expect_doppelganger("plot_cv_dent", plot_cv_dent(test_exp))
})

test_that("plot_cv_dent supports grouping", {
  test_exp <- simple_exp(3, 4)
  test_exp$sample_info$group <- c("A", "A", "B", "B")
  vdiffr::expect_doppelganger("plot_cv_dent_by_group", plot_cv_dent(test_exp, by = "group"))
})

test_that("plot_batch_pca returns ggplot", {
  skip_if_not_installed("factoextra")
  test_exp <- simple_exp(4, 4)
  test_exp$sample_info$batch <- c("A", "A", "B", "B")

  vdiffr::expect_doppelganger("plot_batch_pca", plot_batch_pca(test_exp, batch_col = "batch"))
})

test_that("plot_batch_pca rejects experiment without batch column", {
  skip_if_not_installed("factoextra")
  test_exp <- simple_exp(4, 4)
  expect_error(plot_batch_pca(test_exp), "does not exist")
})

test_that("plot_rep_scatter draws replicate scatter plots", {
  skip_if_not_installed("patchwork")
  test_exp <- simple_exp(5, 6)
  test_exp$sample_info$replicate <- rep(c("A", "B"), each = 3)

  withr::local_seed(123)
  vdiffr::expect_doppelganger(
    "plot_rep_scatter",
    plot_rep_scatter(test_exp, rep_col = "replicate", n_pairs = 2)
  )
})
