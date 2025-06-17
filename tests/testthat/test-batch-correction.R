test_that("correct_batch_effect works with valid batch and group information", {
  # Set seed for reproducible random data
  set.seed(123)
  
  # Create a larger experiment with proper batch and group info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    batch = rep(c("A", "B", "C"), each = 4),
    group = rep(c("Ctrl", "Treat"), 6)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  expr_mat <- matrix(rnorm(120, mean = 10, sd = 2), nrow = 10, ncol = 12)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Should work without error
  result <- correct_batch_effect(exp)
  
  expect_s3_class(result, "glyexp_experiment")
  expect_equal(dim(result$expr_mat), dim(exp$expr_mat))
  expect_equal(colnames(result$expr_mat), colnames(exp$expr_mat))
  expect_equal(rownames(result$expr_mat), rownames(exp$expr_mat))
})

test_that("correct_batch_effect returns original experiment when no batch info", {
  # Create experiment without batch info
  exp <- complex_exp()
  
  # Should return original experiment with info message
  expect_snapshot(result <- correct_batch_effect(exp))
  
  expect_identical(result, exp)
})

test_that("correct_batch_effect warns and returns original when batch and group are confounded", {
  # Create experiment with confounded batch and group
  exp <- complex_exp()
  exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
  exp$sample_info$group <- c("Ctrl", "Ctrl", "Ctrl", "Treat", "Treat", "Treat")
  
  # Should warn and return original experiment
  expect_snapshot(result <- correct_batch_effect(exp))
  
  expect_identical(result, exp)
})

test_that("correct_batch_effect works with batch info but no group info", {
  # Set seed for reproducible random data
  set.seed(456)
  
  # Create a larger experiment with only batch info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    batch = rep(c("A", "B", "C"), each = 4)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  expr_mat <- matrix(rnorm(120, mean = 10, sd = 2), nrow = 10, ncol = 12)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Should work without error
  result <- correct_batch_effect(exp)
  
  expect_s3_class(result, "glyexp_experiment")
  expect_equal(dim(result$expr_mat), dim(exp$expr_mat))
})

test_that("correct_batch_effect validates input", {
  # Should error with invalid input
  expect_error(correct_batch_effect("not_an_experiment"))
  expect_error(correct_batch_effect(123))
})

test_that("correct_batch_effect handles insufficient samples per batch", {
  # Set seed for reproducible random data
  set.seed(789)
  
  # Create experiment with insufficient samples per batch
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:6),
    batch = c("A", "B", "C", "D", "E", "F")  # Only 1 sample per batch
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:5))
  expr_mat <- matrix(rnorm(30, mean = 10, sd = 2), nrow = 5, ncol = 6)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Should warn and return original experiment
  expect_snapshot(result <- correct_batch_effect(exp))
  
  expect_identical(result, exp)
})

test_that("correct_batch_effect preserves experiment structure", {
  # Set seed for reproducible random data
  set.seed(321)
  
  # Create a larger experiment with batch and group info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    batch = rep(c("A", "B", "C"), each = 4),
    group = rep(c("Ctrl", "Treat"), 6)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  expr_mat <- matrix(rnorm(120, mean = 10, sd = 2), nrow = 10, ncol = 12)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  result <- correct_batch_effect(exp)
  
  # Check that sample_info and var_info are preserved
  expect_identical(result$sample_info, exp$sample_info)
  expect_identical(result$var_info, exp$var_info)
  expect_identical(result$meta_data, exp$meta_data)
}) 