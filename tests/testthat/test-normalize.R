test_that("median_normalize works", {
  exp <- simple_exp(3, 3)
  exp <- median_normalize(exp)
  expected <- matrix(c(1/2, 1, 3/2, 4/5, 1, 6/5, 7/8, 1, 9/8), nrow = 3)
  colnames(expected) <- paste0("S", 1:3)
  rownames(expected) <- paste0("V", 1:3)
  expect_equal(exp$expr_mat, expected)
})
