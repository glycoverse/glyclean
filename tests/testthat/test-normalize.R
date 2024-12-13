test_that("median_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- median_normalize(exp)
  expect_snapshot(exp$expr_mat)
})


test_that("median_abs_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- median_abs_normalize(exp)
  expect_snapshot(exp$expr_mat)
})


test_that("total_area_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- total_area_normalize(exp)
  expected <- matrix(c(1/6, 2/6, 3/6, 4/15, 5/15, 6/15, 7/24, 8/24, 9/24), nrow = 3)
  colnames(expected) <- paste0("S", 1:3)
  rownames(expected) <- paste0("V", 1:3)
  expect_equal(exp$expr_mat, expected)
})


test_that("quantile_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- quantile_normalize(exp)
  expect_snapshot(exp$expr_mat)
})


test_that("loessf_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- loessf_normalize(exp)
  expect_snapshot(exp$expr_mat)
})


test_that("loesscyc_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- loesscyc_normalize(exp)
  expect_snapshot(exp$expr_mat)
})


test_that("vsn_normalize works", {
  exp <- simple_exp(42, 3)
  exp <- vsn_normalize(exp)
  expect_snapshot(exp$expr_mat)
})


test_that("median_quotient_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- median_quotient_normalize(exp)
  expected <- matrix(c(1*2.5, 2*2.5, 3*2.5, 4, 5, 6, 7*5/8, 8*5/8, 9*5/8), nrow = 3)
  colnames(expected) <- paste0("S", 1:3)
  rownames(expected) <- paste0("V", 1:3)
  expect_equal(exp$expr_mat, expected)
})


test_that("median_quotient_normalize with `by` specified works", {
  exp <- simple_exp(3, 9)
  # Here we create a matrix that normalization with and without stratification
  # will yield different results.
  # If normalized by group, the matrix will not change.
  expr_mat <- matrix(c(rep(1, 3), rep(2, 3), rep(3, 3)), nrow = 3)
  expr_mat <- cbind(expr_mat, expr_mat, expr_mat)
  colnames(expr_mat) <- paste0("S", 1:9)
  rownames(expr_mat) <- paste0("V", 1:3)
  exp$expr_mat <- expr_mat
  #    S1 S2 S3 S4 S5 S6 S7 S8 S9
  # V1  1  2  3  1  2  3  1  2  3
  # V2  1  2  3  1  2  3  1  2  3
  # V3  1  2  3  1  2  3  1  2  3

  exp$sample_info$group <- rep(c("A", "B", "C"), 3)
  #   sample group
  #   <chr>  <chr>
  # 1 S1     A
  # 2 S2     B
  # 3 S3     C
  # 4 S4     A
  # 5 S5     B
  # 6 S6     C
  # 7 S7     A
  # 8 S8     B
  # 9 S9     C

  normed_exp <- median_quotient_normalize(exp, by = "group")
  expect_equal(normed_exp$expr_mat, expr_mat)
})
