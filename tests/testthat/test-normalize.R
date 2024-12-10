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
