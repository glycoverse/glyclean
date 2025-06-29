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


test_that("impute_zero works with matrix input", {
  # Create test matrix with missing values
  test_mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, ncol = 3)
  rownames(test_mat) <- paste0("V", 1:3)
  colnames(test_mat) <- paste0("S", 1:3)
  
  # Apply imputation
  result_mat <- impute_zero(test_mat)
  
  # Check that the function returns a matrix
  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(rownames(result_mat), rownames(test_mat))
  expect_equal(colnames(result_mat), colnames(test_mat))
  
  # Check that NA values were replaced with 0
  expect_equal(sum(is.na(result_mat)), 0)
  na_positions <- which(is.na(test_mat))
  expect_true(all(result_mat[na_positions] == 0))
})

# Test matrix input with by parameter (new functionality)

test_that("impute_sw_knn works with matrix input and by parameter", {
  skip_if_not_installed("impute")
  
  # Create test matrix with missing values (larger to reduce warnings)
  test_mat <- matrix(runif(40, 1, 100), nrow = 8, ncol = 5)
  test_mat[c(1, 9, 17)] <- NA  # Add fewer missing values
  rownames(test_mat) <- paste0("V", 1:8)
  colnames(test_mat) <- paste0("S", 1:5)
  
  # Create grouping vector
  by_vector <- factor(c("A", "A", "B", "B", "B"))
  
  # Apply imputation with by parameter (suppress technical warnings from impute.knn)
  result_mat <- suppressWarnings(impute_sw_knn(test_mat, k = 2, by = by_vector))
  
  # Check that the function returns a matrix
  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(rownames(result_mat), rownames(test_mat))
  expect_equal(colnames(result_mat), colnames(test_mat))
  
  # Check that no NA values remain
  expect_equal(sum(is.na(result_mat)), 0)
})

test_that("matrix input with by parameter: column name should error for impute functions", {
  # Create test matrix with missing values
  test_mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  test_mat[c(1, 7, 13)] <- NA
  rownames(test_mat) <- paste0("V", 1:4)
  colnames(test_mat) <- paste0("S", 1:5)
  
  # Test with column name (should error for matrix input)
  expect_error(
    impute_sw_knn(test_mat, by = "group"),
    "For matrix input.*must be a vector, not a column name"
  )
})

test_that("matrix input with by parameter: wrong length vector should error for impute functions", {
  # Create test matrix with missing values
  test_mat <- matrix(rnorm(20), nrow = 4, ncol = 5)
  test_mat[c(1, 7, 13)] <- NA
  rownames(test_mat) <- paste0("V", 1:4)
  colnames(test_mat) <- paste0("S", 1:5)
  
  # Test with wrong length vector
  wrong_length_vector <- c("A", "B", "A")  # Should be length 5
  expect_error(
    impute_sw_knn(test_mat, by = wrong_length_vector),
    "vector must have length 5"
  )
})

test_that("impute_min_prob works with matrix input and by parameter", {
  skip_if_not_installed("imputeLCMD")
  
  # Create test matrix with missing values
  test_mat <- matrix(rnorm(24, mean = 10, sd = 2), nrow = 4, ncol = 6)
  test_mat[c(1, 7, 13, 19)] <- NA  # Add some missing values
  rownames(test_mat) <- paste0("V", 1:4)
  colnames(test_mat) <- paste0("S", 1:6)
  
  # Create grouping vector
  by_vector <- rep(c("Control", "Treatment"), each = 3)
  
  # Apply imputation with by parameter
  result_mat <- impute_min_prob(test_mat, by = by_vector)
  
  # Check that the function returns a matrix
  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(rownames(result_mat), rownames(test_mat))
  expect_equal(colnames(result_mat), colnames(test_mat))
  
  # Check that no NA values remain
  expect_equal(sum(is.na(result_mat)), 0)
})
