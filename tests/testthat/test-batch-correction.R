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

# Tests for detect_batch_effect function
test_that("detect_batch_effect works with valid batch information", {
  # Set seed for reproducible random data
  set.seed(123)
  
  # Create a larger experiment with batch info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    batch = rep(c("A", "B", "C"), each = 4)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  
  # Create expression matrix with some batch effect
  expr_mat <- matrix(nrow = 10, ncol = 12)
  for (i in 1:10) {
    # Add different means for different batches to simulate batch effect
    batch_means <- c(8, 10, 12)  # Different means for batches A, B, C
    for (j in 1:12) {
      batch_idx <- ((j - 1) %/% 4) + 1
      expr_mat[i, j] <- rnorm(1, mean = batch_means[batch_idx], sd = 1)
    }
  }
  
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Test detection
  p_values <- detect_batch_effect(exp)
  
  expect_type(p_values, "double")
  expect_length(p_values, nrow(exp$expr_mat))
  expect_named(p_values, rownames(exp$expr_mat))
  expect_true(all(p_values >= 0 & p_values <= 1))
})

test_that("detect_batch_effect works with batch and group information", {
  # Set seed for reproducible random data
  set.seed(456)
  
  # Create experiment with batch and group info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    batch = rep(c("A", "B", "C"), each = 4),
    group = rep(c("Ctrl", "Treat"), 6)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  
  # Create expression matrix
  expr_mat <- matrix(rnorm(120, mean = 10, sd = 2), nrow = 10, ncol = 12)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Test detection with group information
  p_values <- detect_batch_effect(exp, group_col = "group")
  
  expect_type(p_values, "double")
  expect_length(p_values, nrow(exp$expr_mat))
  expect_named(p_values, rownames(exp$expr_mat))
  expect_true(all(p_values >= 0 & p_values <= 1))
})

test_that("detect_batch_effect validates input", {
  # Should error with invalid experiment
  expect_error(detect_batch_effect("not_an_experiment"))
  expect_error(detect_batch_effect(123))
  
  # Should error with non-existent batch column
  exp <- complex_exp()
  expect_error(detect_batch_effect(exp, batch_col = "nonexistent"))
  
  # Should error with non-existent group column
  exp$sample_info$batch <- c("A", "A", "B", "B", "C", "C")
  expect_error(detect_batch_effect(exp, group_col = "nonexistent"))
})

test_that("detect_batch_effect handles insufficient batches", {
  # Create experiment with only one batch
  exp <- complex_exp()
  exp$sample_info$batch <- rep("A", nrow(exp$sample_info))
  
  # Should warn and return vector of 1s
  expect_warning(p_values <- detect_batch_effect(exp))
  
  expect_type(p_values, "double")
  expect_length(p_values, nrow(exp$expr_mat))
  expect_true(all(p_values == 1))
})

test_that("detect_batch_effect returns correct structure", {
  # Set seed for reproducible random data
  set.seed(789)
  
  # Create experiment with batch info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:8),
    batch = rep(c("A", "B"), each = 4)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:5))
  expr_mat <- matrix(rnorm(40, mean = 10, sd = 2), nrow = 5, ncol = 8)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  p_values <- detect_batch_effect(exp)
  
  # Check return structure
  expect_type(p_values, "double")
  expect_length(p_values, 5)
  expect_equal(names(p_values), paste0("V", 1:5))
  expect_true(all(!is.na(p_values)))
  expect_true(all(p_values >= 0 & p_values <= 1))
})

test_that("detect_batch_effect uses custom column names", {
  # Set seed for reproducible random data
  set.seed(321)
  
  # Create experiment with custom column names
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:8),
    my_batch = rep(c("X", "Y"), each = 4),
    my_group = rep(c("Control", "Treatment"), 4)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:5))
  expr_mat <- matrix(rnorm(40, mean = 10, sd = 2), nrow = 5, ncol = 8)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Test with custom column names
  p_values <- detect_batch_effect(exp, batch_col = "my_batch", group_col = "my_group")
  
  expect_type(p_values, "double")
  expect_length(p_values, nrow(exp$expr_mat))
  expect_named(p_values, rownames(exp$expr_mat))
  expect_true(all(p_values >= 0 & p_values <= 1))
}) 