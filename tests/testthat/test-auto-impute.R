test_that("auto_impute works with QC samples", {
  # Create a simple experiment with missing values
  exp <- simple_exp(10, 10)

  # Add some missing values
  exp$expr_mat[1, 1] <- NA
  exp$expr_mat[3, 5] <- NA
  exp$expr_mat[5, 8] <- NA

  # Add QC samples
  exp$sample_info$group <- c(rep("A", 4), rep("B", 4), rep("QC", 2))

  # Verify it runs and picks a method
  expect_snapshot(imputed <- auto_impute(exp, group_col = "group", qc_name = "QC"))
  expect_s3_class(imputed, "glyexp_experiment")

  # Verify no missing values after imputation

  expect_false(any(is.na(imputed$expr_mat)))
})

test_that("auto_impute uses sample_min for small datasets without QC", {
  # Create a small experiment (<=30 samples)
  exp <- simple_exp(10, 20)

  # Add some missing values
  exp$expr_mat[1, 1] <- NA
  exp$expr_mat[3, 5] <- NA

  # Manual application
  manual <- impute_sample_min(exp)

  # Auto application (no QC samples)
  expect_snapshot(auto <- auto_impute(exp, group_col = NULL))

  expect_equal(auto$expr_mat, manual$expr_mat)
})

test_that("auto_impute uses min_prob for medium datasets without QC", {
  # Create a medium experiment (31-100 samples)
  exp <- simple_exp(10, 50)

  # Add some missing values
  exp$expr_mat[1, 1] <- NA
  exp$expr_mat[3, 5] <- NA
  exp$expr_mat[5, 10] <- NA

  # Auto application (no QC samples)
  # We can't compare directly with manual since min_prob has random elements
  expect_snapshot(auto <- auto_impute(exp, group_col = NULL))
  expect_s3_class(auto, "glyexp_experiment")

  # Verify no missing values after imputation
  expect_false(any(is.na(auto$expr_mat)))
})

test_that("auto_impute handles missing group_col gracefully", {
  exp <- simple_exp(10, 10)
  exp$expr_mat[1, 1] <- NA

  # When group_col is NULL, should use default strategy
  expect_snapshot(result <- auto_impute(exp, group_col = NULL))
  expect_s3_class(result, "glyexp_experiment")
  expect_false(any(is.na(result$expr_mat)))
})

test_that("auto_impute handles non-existent group_col gracefully", {
  exp <- simple_exp(10, 10)
  exp$expr_mat[1, 1] <- NA

  # When group_col doesn't exist, should use default strategy
  expect_snapshot(result <- auto_impute(exp, group_col = "nonexistent"))
  expect_s3_class(result, "glyexp_experiment")
  expect_false(any(is.na(result$expr_mat)))
})

test_that("auto_impute validates input arguments", {
  exp <- simple_exp(10, 10)

  # Test invalid exp
  expect_error(auto_impute("not_an_experiment"), "glyexp_experiment")

  # Test invalid to_try
  expect_error(auto_impute(exp, to_try = "not_a_list"), "list")
  expect_error(auto_impute(exp, to_try = list("not_a_function")), "function")
})

test_that("auto_impute works with custom methods", {
  exp <- simple_exp(10, 10)
  exp$expr_mat[1, 1] <- NA
  exp$sample_info$group <- c(rep("A", 4), rep("B", 4), rep("QC", 2))

  # Test with a subset of methods
  custom_methods <- list(
    impute_zero = impute_zero,
    impute_sample_min = impute_sample_min
  )

  expect_snapshot(result <- auto_impute(exp, to_try = custom_methods))
  expect_s3_class(result, "glyexp_experiment")
  expect_false(any(is.na(result$expr_mat)))
})
