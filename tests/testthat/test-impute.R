test_that("zero_impute works", {
  exp <- simple_exp(3, 3)
  exp$expr_mat[1, 1] <- NA
  exp <- zero_impute(exp)
  expect_equal(exp$expr_mat[1, 1], 0)
})


test_that("sample_min_impute works", {
  exp <- simple_exp(3, 3)
  old_expr_mat <- exp$expr_mat
  new_expr_mat <- matrix(
    c(NA, 1, NA,
      2, 2, 4,
      NA, 3, 6),
    nrow = 3, byrow = TRUE
  )
  colnames(new_expr_mat) <- colnames(old_expr_mat)
  rownames(new_expr_mat) <- rownames(old_expr_mat)
  exp$expr_mat <- new_expr_mat

  exp <- sample_min_impute(exp)

  expected <- matrix(
    c(2, 1, 4,
      2, 2, 4,
      2, 3, 6),
    nrow = 3, byrow = TRUE
  )
  colnames(expected) <- colnames(old_expr_mat)
  rownames(expected) <- rownames(old_expr_mat)
  expect_equal(exp$expr_mat, expected)
})


test_that("half_sample_min_impute works", {
  exp <- simple_exp(3, 3)
  old_expr_mat <- exp$expr_mat
  new_expr_mat <- matrix(
    c(NA, 1, NA,
      2, 2, 4,
      NA, 3, 6),
    nrow = 3, byrow = TRUE
  )
  colnames(new_expr_mat) <- colnames(old_expr_mat)
  rownames(new_expr_mat) <- rownames(old_expr_mat)
  exp$expr_mat <- new_expr_mat

  exp <- half_sample_min_impute(exp)

  expected <- matrix(
    c(1, 1, 2,
      2, 2, 4,
      1, 3, 6),
    nrow = 3, byrow = TRUE
  )
  colnames(expected) <- colnames(old_expr_mat)
  rownames(expected) <- rownames(old_expr_mat)
  expect_equal(exp$expr_mat, expected)
})
