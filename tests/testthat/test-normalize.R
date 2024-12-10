test_that("median_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- median_normalize(exp)
  expected <- matrix(c(1/2, 1, 3/2, 4/5, 1, 6/5, 7/8, 1, 9/8), nrow = 3)
  colnames(expected) <- paste0("S", 1:3)
  rownames(expected) <- paste0("V", 1:3)
  expect_equal(exp$expr_mat, expected)
})


test_that("ta_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- ta_normalize(exp)
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
