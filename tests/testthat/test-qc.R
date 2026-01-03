skip_on_ci()
skip_on_cran()

test_that("plot_missing_heatmap works", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_heatmap", plot_missing_heatmap(test_exp))
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