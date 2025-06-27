test_that("normalize_median works", {
  test_exp <- simple_exp(3, 3)
  original_exp <- test_exp
  result_exp <- normalize_median(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that median normalization has been applied correctly
  # After median normalization, each column should have the same median
  col_medians <- apply(result_exp$expr_mat, 2, median)
  expect_true(all(abs(diff(col_medians)) < 1e-10))
})


test_that("normalize_median_abs works", {
  test_exp <- simple_exp(3, 3)
  original_exp <- test_exp
  result_exp <- normalize_median_abs(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that median absolute deviation normalization has been applied
  # After normalization, all columns should have similar scale
  expect_true(all(is.finite(result_exp$expr_mat)))
})


test_that("normalize_total_area works", {
  test_exp <- simple_exp(3, 3)
  original_exp <- test_exp
  result_exp <- normalize_total_area(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # all column sums should be 1
  expect_true(all(abs(colSums(result_exp$expr_mat) - 1) < 1e-10))
})


test_that("normalize_quantile works", {
  test_exp <- simple_exp(10, 10)
  original_exp <- test_exp
  result_exp <- normalize_quantile(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # After quantile normalization, all columns should have identical distributions
  # Check that all columns have the same sorted values
  sorted_cols <- apply(result_exp$expr_mat, 2, sort)
  for (i in 2:ncol(sorted_cols)) {
    expect_equal(sorted_cols[, 1], sorted_cols[, i], tolerance = 1e-10)
  }
})


test_that("normalize_loessf works", {
  test_exp <- simple_exp(10, 10)
  original_exp <- test_exp
  result_exp <- normalize_loessf(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})


test_that("normalize_loesscyc works", {
  test_exp <- simple_exp(10, 10)
  original_exp <- test_exp
  result_exp <- normalize_loesscyc(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})


test_that("normalize_vsn works", {
  test_exp <- simple_exp(50, 50)
  original_exp <- test_exp
  result_exp <- normalize_vsn(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
  
  # VSN should stabilize variance, check that the variance is reasonable
  col_vars <- apply(result_exp$expr_mat, 2, var)
  expect_true(all(col_vars > 0))
})


test_that("normalize_median_quotient works", {
  test_exp <- simple_exp(10, 10)
  original_exp <- test_exp
  result_exp <- normalize_median_quotient(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})





test_that("normalize_median_quotient with `by` specified works", {
  # Create a test experiment with grouping variable
  test_exp <- complex_exp()
  
  # Apply normalization grouped by "group"
  normed_exp <- normalize_median_quotient(test_exp, by = "group")
  
  expect_s3_class(normed_exp, "glyexp_experiment")
  expect_equal(dim(normed_exp$expr_mat), dim(test_exp$expr_mat))
})


test_that("normalize_rlr works", {
  test_exp <- simple_exp(10, 10)
  original_exp <- test_exp
  result_exp <- normalize_rlr(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})


test_that("normalize_rlrma works", {
  test_exp <- simple_exp(10, 10)
  original_exp <- test_exp
  result_exp <- normalize_rlrma(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})


test_that("normalize_rlrmacyc works", {
  test_exp <- simple_exp(10, 10)
  original_exp <- test_exp
  result_exp <- normalize_rlrmacyc(test_exp)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})

# Test new API functionality: factor/vector parameters

test_that("normalize_quantile works with factor by parameter", {
  test_exp <- simple_exp(10, 10)
  
  # Create a factor for grouping
  group_factor <- factor(rep(c("A", "B"), each = 5))
  
  # Apply normalization with factor by parameter
  result_exp <- normalize_quantile(test_exp, by = group_factor)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(test_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(test_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(test_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})

test_that("normalize_quantile works with character vector by parameter", {
  test_exp <- simple_exp(10, 10)
  
  # Create a character vector for grouping
  group_vector <- rep(c("Group1", "Group2"), each = 5)
  
  # Apply normalization with vector by parameter
  result_exp <- normalize_quantile(test_exp, by = group_vector)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(test_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(test_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(test_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})

test_that("normalize_median_quotient works with factor by parameter", {
  test_exp <- simple_exp(6, 6)
  
  # Create a factor for grouping
  group_factor <- factor(rep(c("Control", "Treatment"), each = 3))
  
  # Apply normalization with factor by parameter
  result_exp <- normalize_median_quotient(test_exp, by = group_factor)
  
  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(test_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(test_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(test_exp$expr_mat))
  
  # Check that all values are finite after normalization
  expect_true(all(is.finite(result_exp$expr_mat)))
})

test_that("by parameter validation works correctly", {
  test_exp <- simple_exp(6, 6)
  
  # Test with wrong length vector
  wrong_length_vector <- c("A", "B", "A")  # Should be length 6
  expect_error(
    normalize_quantile(test_exp, by = wrong_length_vector),
    "vector must have length 6"
  )
  
  # Test with numeric vector (should work)
  numeric_vector <- rep(c(1, 2), each = 3)
  result_exp <- normalize_quantile(test_exp, by = numeric_vector)
  expect_s3_class(result_exp, "glyexp_experiment")
})

test_that("by parameter comparison: column name vs factor", {
  # Create experiment with group column
  test_exp <- simple_exp(6, 6)
  test_exp$sample_info$group <- rep(c("A", "B"), each = 3)
  
  # Normalize using column name
  result_by_name <- normalize_quantile(test_exp, by = "group")
  
  # Normalize using factor directly
  group_factor <- factor(rep(c("A", "B"), each = 3))
  result_by_factor <- normalize_quantile(test_exp, by = group_factor)
  
  # Results should be identical (within numerical tolerance)
  expect_equal(result_by_name$expr_mat, result_by_factor$expr_mat, tolerance = 1e-10)
})
