test_that("plot_missing_bar orders samples by missing proportion", {
  skip_on_ci()
  skip_on_cran()

  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[1:2, 2] <- NA

  vdiffr::expect_doppelganger("plot_missing_bar", plot_missing_bar(test_exp))
})
