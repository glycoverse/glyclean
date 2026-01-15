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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Should work without error
  suppressMessages(result <- correct_batch_effect(exp))
  
  expect_s3_class(result, "glyexp_experiment")
  expect_equal(dim(result$expr_mat), dim(exp$expr_mat))
  expect_equal(colnames(result$expr_mat), colnames(exp$expr_mat))
  expect_equal(rownames(result$expr_mat), rownames(exp$expr_mat))
})

test_that("correct_batch_effect errors when no batch column exists", {
  # Create experiment without batch info
  exp <- complex_exp()
  
  # Should error when batch column doesn't exist
  expect_error(
    correct_batch_effect(exp),
    "The column \"batch\" does not exist in `sample_info`"
  )
})

test_that("correct_batch_effect warns and returns original when batch and group are confounded", {
  # Create experiment with confounded batch and group
  exp <- complex_exp()
  exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
  exp$sample_info$group <- c("Ctrl", "Ctrl", "Ctrl", "Treat", "Treat", "Treat")
  
  # Should warn and return original experiment when group_col is specified
  expect_snapshot(result <- correct_batch_effect(exp, group = "group"))
  
  expect_identical(result, exp)
})

test_that("correct_batch_effect works when group column exists but group_col not specified", {
  # Create experiment with batch and group columns
  exp <- complex_exp()
  exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
  exp$sample_info$group <- c("Ctrl", "Ctrl", "Ctrl", "Treat", "Treat", "Treat")
  
  # Should work when group_col is not specified (ignores group column)
  suppressMessages(result <- correct_batch_effect(exp))
  
  expect_s3_class(result, "glyexp_experiment")
  expect_equal(dim(result$expr_mat), dim(exp$expr_mat))
  # Result should be different from original since batch correction was applied
  expect_false(identical(result$expr_mat, exp$expr_mat))
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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Should work without error
  suppressMessages(result <- correct_batch_effect(exp))
  
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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Test with group_col specified to use both batch and group information
  suppressMessages(result <- correct_batch_effect(exp, group = "group"))
  
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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Test detection
  suppressMessages(p_values <- detect_batch_effect(exp))
  
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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Test detection with group information
  suppressMessages(p_values <- detect_batch_effect(exp, group = "group"))
  
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
  expect_error(detect_batch_effect(exp, group = "nonexistent"))
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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  suppressMessages(p_values <- detect_batch_effect(exp))
  
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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Test with custom column names
  suppressMessages(p_values <- detect_batch_effect(exp, batch = "my_batch", group = "my_group"))
  
  expect_type(p_values, "double")
  expect_length(p_values, nrow(exp$expr_mat))
  expect_named(p_values, rownames(exp$expr_mat))
  expect_true(all(p_values >= 0 & p_values <= 1))
})

# Mathematical validation tests for detect_batch_effect
test_that("detect_batch_effect correctly identifies batch effects - mathematical validation", {
  set.seed(42)
  n_vars <- 20
  n_samples_per_batch <- 6
  n_batches <- 3
  n_samples <- n_samples_per_batch * n_batches
  
  # Create sample info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:n_samples),
    batch = rep(paste0("Batch", 1:n_batches), each = n_samples_per_batch)
  )
  
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))
  
  # Create expression matrix with controlled batch effects
  expr_mat <- matrix(nrow = n_vars, ncol = n_samples)
  rownames(expr_mat) <- var_info$variable
  colnames(expr_mat) <- sample_info$sample
  
  # First 10 variables have strong batch effects
  # Different means for each batch: Batch1=5, Batch2=10, Batch3=15
  batch_means <- c(5, 10, 15)
  for (i in 1:10) {
    for (j in 1:n_samples) {
      batch_idx <- ceiling(j / n_samples_per_batch)
      expr_mat[i, j] <- rnorm(1, mean = batch_means[batch_idx], sd = 1)
    }
  }
  
  # Last 10 variables have no batch effects (same mean across batches)
  for (i in 11:20) {
    expr_mat[i, ] <- rnorm(n_samples, mean = 8, sd = 1)
  }
  
  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Test detection
  suppressMessages(p_values <- detect_batch_effect(exp))
  
  # Variables 1-10 should have significant batch effects (small p-values)
  vars_with_batch_effect <- p_values[1:10]
  vars_without_batch_effect <- p_values[11:20]
  
  # Most variables with batch effects should have p < 0.05
  expect_true(mean(vars_with_batch_effect < 0.05) > 0.7,
              info = "Most variables with batch effects should be detected")
  
  # Most variables without batch effects should have p >= 0.05
  expect_true(mean(vars_without_batch_effect >= 0.05) > 0.7,
              info = "Most variables without batch effects should not be detected")
  
  # Mean p-value for batch effect variables should be smaller
  expect_true(mean(vars_with_batch_effect, na.rm = TRUE) < 
              mean(vars_without_batch_effect, na.rm = TRUE),
              info = "Mean p-value should be smaller for variables with batch effects")
})

test_that("detect_batch_effect with group covariate - mathematical validation", {
  set.seed(123)
  n_vars <- 10
  n_samples <- 12
  
  # Create balanced design with batch and group
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:n_samples),
    batch = rep(c("A", "B"), each = 6),
    group = rep(c("Ctrl", "Treat"), 6)  # Balanced design
  )
  
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))
  expr_mat <- matrix(nrow = n_vars, ncol = n_samples)
  rownames(expr_mat) <- var_info$variable
  colnames(expr_mat) <- sample_info$sample
  
  # Create data with both batch and group effects
  for (i in 1:n_vars) {
    for (j in 1:n_samples) {
      batch_effect <- ifelse(sample_info$batch[j] == "A", 2, 0)
      group_effect <- ifelse(sample_info$group[j] == "Treat", 3, 0)
      expr_mat[i, j] <- rnorm(1, mean = 10 + batch_effect + group_effect, sd = 0.5)
    }
  }
  
  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Test without group covariate (should detect both batch and group effects)
  suppressMessages(p_values_no_group <- detect_batch_effect(exp))
  
  # Test with group covariate (should detect only batch effects)
  suppressMessages(p_values_with_group <- detect_batch_effect(exp, group = "group"))
  
  # Both methods should detect some significant effects, but we're more flexible
  significant_no_group <- sum(p_values_no_group < 0.05, na.rm = TRUE)
  significant_with_group <- sum(p_values_with_group < 0.05, na.rm = TRUE)
  
  # At least some variables should show significant effects
  expect_true(significant_no_group >= 3,
              info = "Should detect some significant effects without group covariate")
  expect_true(significant_with_group >= 3,
              info = "Should detect some significant batch effects with group covariate")
})

# Mathematical validation tests for correct_batch_effect
test_that("correct_batch_effect effectively removes batch effects - mathematical validation", {
  set.seed(456)
  n_vars <- 50
  n_samples_per_batch <- 8
  n_batches <- 3
  n_samples <- n_samples_per_batch * n_batches
  
  # Create sample info with batch and group
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:n_samples),
    batch = rep(paste0("B", 1:n_batches), each = n_samples_per_batch),
    group = rep(c("Ctrl", "Treat"), n_samples / 2)
  )
  
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))
  
  # Create expression data with strong batch effects and group effects
  expr_mat <- matrix(nrow = n_vars, ncol = n_samples)
  rownames(expr_mat) <- var_info$variable
  colnames(expr_mat) <- sample_info$sample
  
  # Add batch effects (different means) and group effects
  batch_means <- c(8, 12, 16)  # Strong batch effects
  for (i in 1:n_vars) {
    for (j in 1:n_samples) {
      batch_idx <- ceiling(j / n_samples_per_batch)
      batch_effect <- batch_means[batch_idx]
      group_effect <- ifelse(sample_info$group[j] == "Treat", 4, 0)
      expr_mat[i, j] <- rnorm(1, mean = batch_effect + group_effect, sd = 1)
    }
  }
  
  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Detect batch effects before correction
  suppressMessages(p_values_before <- detect_batch_effect(exp, group = "group"))
  
  # Apply batch correction
  suppressMessages(corrected_exp <- correct_batch_effect(exp))
  
  # Detect batch effects after correction
  suppressMessages(p_values_after <- detect_batch_effect(corrected_exp, group = "group"))
  
  # Before correction: should detect significant batch effects
  significant_before <- sum(p_values_before < 0.05, na.rm = TRUE)
  expect_true(significant_before > n_vars * 0.5,
              info = "Should detect significant batch effects before correction")
  
  # After correction: batch effects should be substantially reduced
  significant_after <- sum(p_values_after < 0.05, na.rm = TRUE)
  
  # Check that correction was effective
  expect_true(significant_after < significant_before * 0.5,
              info = "Should substantially reduce detected batch effects")
  
  # Mean p-values should increase (indicating weaker batch effects)
  mean_p_before <- mean(p_values_before, na.rm = TRUE)
  mean_p_after <- mean(p_values_after, na.rm = TRUE)
  
  expect_true(mean_p_after > mean_p_before,
              info = "Mean p-values should increase after batch correction")
})

test_that("correct_batch_effect preserves group differences - mathematical validation", {
  set.seed(789)
  n_vars <- 30
  n_samples_per_group_per_batch <- 4
  n_batches <- 3
  n_samples <- n_samples_per_group_per_batch * 2 * n_batches  # 2 groups
  
  # Create balanced design
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:n_samples),
    batch = rep(paste0("B", 1:n_batches), each = n_samples_per_group_per_batch * 2),
    group = rep(rep(c("Ctrl", "Treat"), each = n_samples_per_group_per_batch), n_batches)
  )
  
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))
  
  # Create data with both batch and group effects
  expr_mat <- matrix(nrow = n_vars, ncol = n_samples)
  rownames(expr_mat) <- var_info$variable
  colnames(expr_mat) <- sample_info$sample
  
  true_group_effect <- 5  # True biological difference
  batch_effects <- c(0, 3, 6)  # Batch effects to be removed
  
  for (i in 1:n_vars) {
    for (j in 1:n_samples) {
      batch_idx <- ceiling(j / (n_samples_per_group_per_batch * 2))
      batch_effect <- batch_effects[batch_idx]
      group_effect <- ifelse(sample_info$group[j] == "Treat", true_group_effect, 0)
      expr_mat[i, j] <- rnorm(1, mean = 10 + batch_effect + group_effect, sd = 0.8)
    }
  }
  
  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Calculate group differences before and after correction
  ctrl_samples <- sample_info$sample[sample_info$group == "Ctrl"]
  treat_samples <- sample_info$sample[sample_info$group == "Treat"]
  
  # Before correction
  ctrl_means_before <- rowMeans(exp$expr_mat[, ctrl_samples])
  treat_means_before <- rowMeans(exp$expr_mat[, treat_samples])
  group_diff_before <- treat_means_before - ctrl_means_before
  
  # Apply correction
  suppressMessages(corrected_exp <- correct_batch_effect(exp))
  
  # After correction
  ctrl_means_after <- rowMeans(corrected_exp$expr_mat[, ctrl_samples])
  treat_means_after <- rowMeans(corrected_exp$expr_mat[, treat_samples])
  group_diff_after <- treat_means_after - ctrl_means_after
  
  # Group differences should be preserved (correlation should be reasonable)
  correlation <- cor(group_diff_before, group_diff_after)
  expect_true(correlation > 0.5,
              info = "Group differences should be reasonably preserved after batch correction")
  
  # Mean group difference should be in reasonable range
  mean_group_diff_after <- mean(group_diff_after)
  expect_true(abs(mean_group_diff_after - true_group_effect) < 2,
              info = "Mean group difference should be reasonably close to true biological effect")
})

test_that("batch correction reduces batch-related variance", {
  set.seed(321)
  n_vars <- 20
  n_samples_per_batch <- 6
  n_batches <- 3
  n_samples <- n_samples_per_batch * n_batches
  
  # Create sample info
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:n_samples),
    batch = rep(paste0("B", 1:n_batches), each = n_samples_per_batch)
  )
  
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))
  
  # Create data with batch effects
  expr_mat <- matrix(nrow = n_vars, ncol = n_samples)
  rownames(expr_mat) <- var_info$variable
  colnames(expr_mat) <- sample_info$sample
  
  batch_means <- c(5, 10, 15)  # Different batch means
  for (i in 1:n_vars) {
    for (j in 1:n_samples) {
      batch_idx <- ceiling(j / n_samples_per_batch)
      expr_mat[i, j] <- rnorm(1, mean = batch_means[batch_idx], sd = 1)
    }
  }
  
  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Calculate batch-related variance before correction
  batch_means_before <- sapply(1:n_batches, function(b) {
    batch_samples <- sample_info$sample[sample_info$batch == paste0("B", b)]
    rowMeans(exp$expr_mat[, batch_samples])
  })
  
  batch_variance_before <- apply(batch_means_before, 1, var)
  
  # Apply correction
  suppressMessages(corrected_exp <- correct_batch_effect(exp))
  
  # Calculate batch-related variance after correction
  batch_means_after <- sapply(1:n_batches, function(b) {
    batch_samples <- sample_info$sample[sample_info$batch == paste0("B", b)]
    rowMeans(corrected_exp$expr_mat[, batch_samples])
  })
  
  batch_variance_after <- apply(batch_means_after, 1, var)
  
  # Batch-related variance should be substantially reduced
  variance_reduction <- mean(batch_variance_after / batch_variance_before)
  expect_true(variance_reduction < 0.1,
              info = "Batch-related variance should be substantially reduced")
  
  # Most variables should show substantial reduction in batch variance
  reduced_vars <- sum(batch_variance_after < batch_variance_before * 0.5)
  expect_true(reduced_vars > n_vars * 0.8,
              info = "Most variables should show substantial reduction in batch variance")
})

test_that("correct_batch_effect works with matrix input", {
  # Set seed for reproducible results
  set.seed(123)
  
  # Create test matrix
  test_mat <- matrix(rnorm(60, mean = 10, sd = 2), nrow = 6, ncol = 10)
  rownames(test_mat) <- paste0("V", 1:6)
  colnames(test_mat) <- paste0("S", 1:10)
  
  # Create batch and group factors
  batch_factor <- factor(rep(c("A", "B"), each = 5))
  group_factor <- factor(rep(c("Ctrl", "Treat"), times = 5))
  
  # Apply batch correction
  suppressMessages(result_mat <- correct_batch_effect(test_mat, batch = batch_factor, group = group_factor))
  
  # Check that the function returns a matrix
  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(rownames(result_mat), rownames(test_mat))
  expect_equal(colnames(result_mat), colnames(test_mat))
  
  # Check that all values are finite
  expect_true(all(is.finite(result_mat)))
})

test_that("detect_batch_effect works with matrix input", {
  # Set seed for reproducible results
  set.seed(456)
  
  # Create test matrix with batch effects
  test_mat <- matrix(nrow = 5, ncol = 8)
  batch_factor <- factor(rep(c("A", "B"), each = 4))
  
  # Add artificial batch effects to some variables
  for (i in 1:5) {
    batch_a_vals <- rnorm(4, mean = 10, sd = 1)
    batch_b_vals <- rnorm(4, mean = 12, sd = 1)  # Different mean for batch B
    test_mat[i, ] <- c(batch_a_vals, batch_b_vals)
  }
  
  rownames(test_mat) <- paste0("V", 1:5)
  colnames(test_mat) <- paste0("S", 1:8)
  
  # Detect batch effects
  suppressMessages(p_values <- detect_batch_effect(test_mat, batch = batch_factor))
  
  # Check that the function returns appropriate results
  expect_type(p_values, "double")
  expect_length(p_values, nrow(test_mat))
  expect_named(p_values, rownames(test_mat))
  expect_true(all(p_values >= 0 & p_values <= 1))
})

test_that("batch correction functions reject column names for matrix input", {
  # Set seed for reproducible results
  set.seed(789)
  
  # Create test matrix with larger, more stable values
  test_mat <- matrix(rnorm(60, mean = 100, sd = 10), nrow = 6, ncol = 10)
  rownames(test_mat) <- paste0("V", 1:6)
  colnames(test_mat) <- paste0("S", 1:10)
  
  # Create valid factor vectors
  batch_factor <- factor(rep(c("A", "B"), each = 5))
  group_factor <- factor(rep(c("Ctrl", "Treat"), times = 5))
  
  # Test that column names are rejected for batch parameter
  expect_error(
    correct_batch_effect(test_mat, batch = "batch"),
    "Column name 'batch' provided for.*batch.*but no sample_info available.*Please provide a factor or vector instead"
  )
  
  expect_error(
    detect_batch_effect(test_mat, batch = "batch"),
    "Column name 'batch' provided for.*batch.*but no sample_info available.*Please provide a factor or vector instead"
  )
  
  # Test that column names are rejected for group parameter
  expect_error(
    correct_batch_effect(test_mat, batch = batch_factor, group = "group"),
    "Column name 'group' provided for.*group.*but no sample_info available.*Please provide a factor or vector instead"
  )
  
  expect_error(
    detect_batch_effect(test_mat, batch = batch_factor, group = "group"),
    "Column name 'group' provided for.*group.*but no sample_info available.*Please provide a factor or vector instead"
  )
  
  # Test that factor vectors work correctly (use expect_warning to capture NaN warnings if they occur)
  expect_warning(
    {
      result1 <- suppressMessages(correct_batch_effect(test_mat, batch = batch_factor, group = group_factor))
      result2 <- suppressMessages(detect_batch_effect(test_mat, batch = batch_factor, group = group_factor))
    },
    NA  # Expect no warnings, but if NaN warnings occur they will be captured
  )
})

test_that("correct_batch_effect handles missing batch column gracefully", {
  # Create experiment without batch column
  exp <- complex_exp()
  
  # Test with non-existent batch column - should throw error as expected
  expect_error(
    correct_batch_effect(exp, batch = "non_existent_batch"),
    "The column \"non_existent_batch\" does not exist in `sample_info`"
  )
})

test_that("correct_batch_effect handles single batch scenario", {
  # Create experiment with only one batch
  exp <- complex_exp()
  exp$sample_info$batch <- rep("A", nrow(exp$sample_info))
  
  # Should handle single batch gracefully with warning
  expect_warning(
    result <- correct_batch_effect(exp, batch = "batch"),
    "Less than 2 batches found"
  )
  expect_s3_class(result, "glyexp_experiment")
  expect_identical(result, exp)  # Should return original when correction fails
})

test_that("correct_batch_effect handles matrix with single batch", {
  # Create test matrix
  test_mat <- matrix(rnorm(30, mean = 10, sd = 2), nrow = 5, ncol = 6)
  rownames(test_mat) <- paste0("V", 1:5)
  colnames(test_mat) <- paste0("S", 1:6)
  
  # Create single batch vector
  single_batch <- factor(rep("A", 6))
  
  # Should return original matrix for single batch
  result <- suppressMessages(correct_batch_effect(test_mat, batch = single_batch))
  expect_equal(result, test_mat)
})

test_that("detect_batch_effect handles missing batch column gracefully", {
  # Create experiment without batch column  
  exp <- complex_exp()
  
  # Should throw error when batch column doesn't exist
  expect_error(
    detect_batch_effect(exp, batch = "non_existent_batch"),
    "The column \"non_existent_batch\" does not exist in `sample_info`"
  )
})

test_that("batch functions handle default batch column missing", {
  # Create experiment without default 'batch' column
  exp <- complex_exp()
  
  # Test with default batch parameter when column doesn't exist
  expect_error(
    correct_batch_effect(exp),  # Uses default batch = "batch"
    "The column \"batch\" does not exist in `sample_info`"
  )
  
  expect_error(
    detect_batch_effect(exp),  # Uses default batch = "batch"
    "The column \"batch\" does not exist in `sample_info`"
  )
})

test_that("detect_batch_effect handles single batch scenario", {
  # Create experiment with only one batch
  exp <- complex_exp()
  exp$sample_info$batch <- rep("A", nrow(exp$sample_info))
  
  # Should handle single batch with warning and return vector of 1s
  expect_warning(
    result <- detect_batch_effect(exp, batch = "batch"),
    "Less than 2 batches found"
  )
  expect_type(result, "double")
  expect_length(result, nrow(exp$expr_mat))
  expect_true(all(result == 1))
})

test_that("detect_batch_effect handles matrix with single batch", {
  # Create test matrix
  test_mat <- matrix(rnorm(30, mean = 10, sd = 2), nrow = 5, ncol = 6)
  rownames(test_mat) <- paste0("V", 1:5)
  colnames(test_mat) <- paste0("S", 1:6)
  
  # Create single batch vector
  single_batch <- factor(rep("A", 6))
  
  # Should return vector of 1s for single batch with warning
  expect_warning(
    result <- detect_batch_effect(test_mat, batch = single_batch),
    "Less than 2 batches found"
  )
  expect_type(result, "double")
  expect_length(result, nrow(test_mat))
  expect_true(all(result == 1))
})

test_that("batch correction handles direct vector inputs for experiments", {
  # Create experiment with batch column
  exp <- complex_exp()
  exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")
  
  # Test with direct factor vector instead of column name
  batch_vector <- factor(exp$sample_info$batch)
  group_vector <- factor(exp$sample_info$group)
  
  suppressMessages(result1 <- correct_batch_effect(exp, batch = batch_vector))
  
  # This combination may be confounded, so expect warning
  expect_warning(
    result2 <- correct_batch_effect(exp, batch = batch_vector, group = group_vector),
    "Batch and group variables are highly confounded"
  )
  
  expect_s3_class(result1, "glyexp_experiment")
  expect_s3_class(result2, "glyexp_experiment")
  
  # Detect batch effect with direct vectors
  p_values1 <- suppressMessages(detect_batch_effect(exp, batch = batch_vector))
  p_values2 <- suppressMessages(detect_batch_effect(exp, batch = batch_vector, group = group_vector))
  
  expect_type(p_values1, "double")
  expect_type(p_values2, "double")
  expect_length(p_values1, nrow(exp$expr_mat))
  expect_length(p_values2, nrow(exp$expr_mat))
})

test_that("batch correction handles NULL batch parameter", {
  # Create experiment
  exp <- complex_exp()
  
  # Test with NULL batch - should trigger error for detect_batch_effect
  expect_error(detect_batch_effect(exp, batch = NULL), "Batch information is required")
  expect_error(correct_batch_effect(exp, batch = NULL), "Batch information is required")
})

test_that("batch correction handles vector length mismatch", {
  # Create experiment
  exp <- complex_exp()
  
  # Create vectors with wrong length
  wrong_batch <- factor(c("A", "B"))  # Too short
  wrong_group <- factor(c("Ctrl", "Treat"))  # Too short
  
  expect_error(
    correct_batch_effect(exp, batch = wrong_batch),
    "vector must have length"
  )
  
  expect_error(
    detect_batch_effect(exp, batch = wrong_batch),
    "vector must have length"
  )
  
  # Test with correct batch but wrong group length
  correct_batch <- factor(rep(c("A", "B"), length.out = ncol(exp$expr_mat)))
  expect_error(
    correct_batch_effect(exp, batch = correct_batch, group = wrong_group),
    "vector must have length"
  )
})

test_that("batch correction with extreme data values", {
  # Test with data that might cause ComBat to fail
  set.seed(42)
  
  # Create experiment with very small variance (might cause ComBat issues)
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:8),
    batch = rep(c("A", "B"), each = 4)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:5))
  
  # Create expression matrix with very small variance
  expr_mat <- matrix(10 + rnorm(40, mean = 0, sd = 0.001), nrow = 5, ncol = 8)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable

  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )
  
  # Should handle extreme cases gracefully (may return original or corrected)
  result <- suppressMessages(correct_batch_effect(exp))
  expect_s3_class(result, "glyexp_experiment")
  expect_equal(dim(result$expr_mat), dim(exp$expr_mat))
})

test_that("batch correction error handling for invalid inputs", {
  # Test with non-matrix, non-experiment input
  expect_error(correct_batch_effect("invalid_input"))
  expect_error(detect_batch_effect("invalid_input"))
  expect_error(correct_batch_effect(123))
  expect_error(detect_batch_effect(123))

  # Test with list input
  expect_error(correct_batch_effect(list(a = 1, b = 2)))
  expect_error(detect_batch_effect(list(a = 1, b = 2)))
})

# Tests for limma method
test_that("correct_batch_effect works with limma method", {
  set.seed(123)

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
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )

  # Should work without error
  suppressMessages(result <- correct_batch_effect(exp, method = "limma"))

  expect_s3_class(result, "glyexp_experiment")
  expect_equal(dim(result$expr_mat), dim(exp$expr_mat))
  expect_equal(colnames(result$expr_mat), colnames(exp$expr_mat))
  expect_equal(rownames(result$expr_mat), rownames(exp$expr_mat))
  # Result should be different from original since batch correction was applied
  expect_false(identical(result$expr_mat, exp$expr_mat))
})

test_that("correct_batch_effect works with limma method and matrix input", {
  set.seed(456)

  test_mat <- matrix(rnorm(60, mean = 10, sd = 2), nrow = 6, ncol = 10)
  rownames(test_mat) <- paste0("V", 1:6)
  colnames(test_mat) <- paste0("S", 1:10)

  batch_factor <- factor(rep(c("A", "B"), each = 5))
  group_factor <- factor(rep(c("Ctrl", "Treat"), times = 5))

  suppressMessages(result_mat <- correct_batch_effect(test_mat, batch = batch_factor, group = group_factor, method = "limma"))

  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(rownames(result_mat), rownames(test_mat))
  expect_equal(colnames(result_mat), colnames(test_mat))
  expect_true(all(is.finite(result_mat)))
})

test_that("correct_batch_effect validates method parameter", {
  exp <- glyexp::toy_experiment
  exp$sample_info$batch <- c("A", "A", "A", "B", "B", "B")

  # Should error with invalid method
  expect_error(
    correct_batch_effect(exp, method = "invalid"),
    "arg.*should be one of"
  )
})

 