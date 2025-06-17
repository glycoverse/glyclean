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


test_that("data can be loaded", {
  expect_no_error(test_exp <<- simple_exp(10, 10))
  # Set seed for reproducible random sampling
  set.seed(999)
  # Add some NA values for testing
  test_exp$expr_mat[sample(length(test_exp$expr_mat), 20)] <- NA
  expect_no_error(old_test_exp <<- test_exp)
})


test_that("impute_zero works", {
  result_exp <- impute_zero(test_exp)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
  # Check that NA values were replaced with 0
  na_positions <- which(is.na(test_exp$expr_mat))
  expect_true(all(result_exp$expr_mat[na_positions] == 0))
})


test_that("impute_sample_min works", {
  result_exp <- impute_sample_min(old_test_exp)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
  
  # Test that the filled values are actually the minimum values in each column
  old_mat <- old_test_exp$expr_mat
  new_mat <- result_exp$expr_mat
  
  for (i in seq_len(ncol(old_mat))) {
    if (any(is.na(old_mat[, i]))) {
      col_min <- min(old_mat[, i], na.rm = TRUE)
      old_na_indices <- which(is.na(old_mat[, i]))
      filled_values <- new_mat[old_na_indices, i]
      expect_true(all(filled_values == col_min))
    }
  }
})


test_that("impute_half_sample_min works", {
  result_exp <- impute_half_sample_min(old_test_exp)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
  
  # Test that the filled values are actually half the minimum values in each column
  old_mat <- old_test_exp$expr_mat
  new_mat <- result_exp$expr_mat
  
  for (i in seq_len(ncol(old_mat))) {
    if (any(is.na(old_mat[, i]))) {
      col_min <- min(old_mat[, i], na.rm = TRUE)
      old_na_indices <- which(is.na(old_mat[, i]))
      filled_values <- new_mat[old_na_indices, i]
      expect_true(all(filled_values == col_min / 2))
    }
  }
})


test_that("impute_sw_knn works", {
  skip_if_not_installed("impute")
  result_exp <- impute_sw_knn(test_exp, k = 5)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
})


test_that("impute_fw_knn works", {
  skip_if_not_installed("impute")
  result_exp <- impute_fw_knn(test_exp, k = 5)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
})


test_that("impute_bpca works", {
  skip_if_not_installed("pcaMethods")
  result_exp <- impute_bpca(test_exp)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
})


# test_that("impute_ppca works", {
#   skip_if_not_installed("pcaMethods")
#   result_exp <- impute_ppca(test_exp)
#   expect_equal(sum(is.na(result_exp$expr_mat)), 0)
# })


test_that("impute_svd works", {
  skip_if_not_installed("pcaMethods")
  result_exp <- impute_svd(test_exp)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
})


test_that("impute_min_prob works", {
  skip_if_not_installed("imputeLCMD")
  result_exp <- impute_min_prob(test_exp)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
})


test_that("impute_miss_forest works", {
  skip_if_not_installed("missForest")
  result_exp <- impute_miss_forest(test_exp)
  expect_equal(sum(is.na(result_exp$expr_mat)), 0)
})
