# Test main logic path for glycoproteomics data
test_that("auto_clean works for glycoproteomics data", {
  test_exp <- complex_exp()
  result_exp <- auto_clean(test_exp)
  
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycoproteomics")
  expect_false(any(is.na(result_exp$expr_mat)))
})

# Test main logic path for glycomics data
test_that("auto_clean works for glycomics data", {
  # Create proper glycomics experiment
  sample_info <- tibble::tibble(sample = paste0("S", 1:10))
  var_info <- tibble::tibble(variable = paste0("V", 1:8))
  expr_mat <- matrix(runif(80), nrow = 8)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  test_exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycomics",
    glycan_type = "N"
  )
  
  result_exp <- auto_clean(test_exp)
  
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycomics")
  expect_false(any(is.na(result_exp$expr_mat)))
})

# Test different imputation methods based on sample size
test_that("auto_clean handles different sample sizes", {
  # Small dataset (≤30 samples) - triggers impute_sample_min
  small_exp <- complex_exp()
  # Add some missing values
  small_exp$expr_mat[1:2, 1:2] <- NA
  result_small <- auto_clean(small_exp)
  expect_false(any(is.na(result_small$expr_mat)))
  
  # Just test that the function works with the complex experiment
  # The exact imputation method testing would require larger datasets
  expect_s3_class(result_small, "glyexp_experiment")
})

# Test parameter validation
test_that("auto_clean validates parameters", {
  test_exp <- complex_exp()
  
  expect_error(auto_clean("not_an_experiment"))
  expect_error(auto_clean(test_exp, to_level = "invalid"))
})
