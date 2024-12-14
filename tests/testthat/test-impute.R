missing_exp_3_3 <- function() {
  exp <- simple_exp(3, 3)
  old_expr_mat <- exp$expr_mat
  new_expr_mat <- matrix(
    c(1, 1, NA,
      NA, 2, 4,
      1, 3, 6),
    nrow = 3, byrow = TRUE
  )
  colnames(new_expr_mat) <- colnames(old_expr_mat)
  rownames(new_expr_mat) <- rownames(old_expr_mat)
  exp$expr_mat <- new_expr_mat
  exp
}


missing_exp_10_10 <- function() {
  exp <- simple_exp(10, 10)
  exp$expr_mat[c(1, 5, 29, 68, 45, 100)] <- NA
  exp
}


test_that("zero_impute works", {
  exp <- simple_exp(3, 3)
  exp$expr_mat[1, 1] <- NA
  exp <- zero_impute(exp)
  expect_equal(exp$expr_mat[1, 1], 0)
})


test_that("sample_min_impute works", {
  old_exp <- missing_exp_3_3()
  exp <- sample_min_impute(old_exp)
  expected <- matrix(
    c(1, 1, 4,
      1, 2, 4,
      1, 3, 6),
    nrow = 3, byrow = TRUE
  )
  colnames(expected) <- colnames(old_exp$expr_mat)
  rownames(expected) <- rownames(old_exp$expr_mat)
  expect_equal(exp$expr_mat, expected)
})


test_that("half_sample_min_impute works", {
  old_exp <- missing_exp_3_3()
  exp <- half_sample_min_impute(old_exp)
  expected <- matrix(
    c(1, 1, 2,
      0.5, 2, 4,
      1, 3, 6),
    nrow = 3, byrow = TRUE
  )
  colnames(expected) <- colnames(old_exp$expr_mat)
  rownames(expected) <- rownames(old_exp$expr_mat)
  expect_equal(exp$expr_mat, expected)
})


test_that("sw_knn_impute works", {
  exp <- missing_exp_10_10()
  exp <- sw_knn_impute(exp, k = 5)

  expect_snapshot(round(exp$expr_mat, 4))
})


test_that("fw_knn_impute works", {
  exp <- missing_exp_10_10()
  exp <- fw_knn_impute(exp, k = 5)

  expect_snapshot(round(exp$expr_mat, 4))
})


test_that("bpca_impute works", {
  exp <- missing_exp_10_10()
  exp <- bpca_impute(exp)

  expect_snapshot(round(exp$expr_mat, 4))
})


# test_that("ppca_impute works", {
#   exp <- missing_exp_10_10()
#   exp <- ppca_impute(exp)
#
#   expect_snapshot(round(exp$expr_mat, 4))
# })


test_that("svd_impute works", {
  exp <- missing_exp_10_10()
  exp <- svd_impute(exp)

  expect_snapshot(round(exp$expr_mat, 4))
})


test_that("min_prob_impute works", {
  exp <- missing_exp_10_10()
  exp <- min_prob_impute(exp)

  expect_snapshot(round(exp$expr_mat, 4))
})


test_that("miss_forest_impute works", {
  exp <- missing_exp_10_10()
  exp <- miss_forest_impute(exp)

  expect_snapshot(round(exp$expr_mat, 4))
})
