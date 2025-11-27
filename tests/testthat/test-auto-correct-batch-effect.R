test_that("auto_correct_batch_effect performs correction when threshold exceeded", {
  set.seed(123)

  # Create experiment with strong batch effects
  n_vars <- 20
  n_samples <- 12
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:n_samples),
    batch = rep(c("A", "B"), each = 6),
    group = rep(c("Ctrl", "Treat"), 6)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))

  expr_mat <- matrix(nrow = n_vars, ncol = n_samples)
  rownames(expr_mat) <- var_info$variable
  colnames(expr_mat) <- sample_info$sample

  # Add strong batch effects to all variables
  for (i in 1:n_vars) {
    batch_effect <- ifelse(sample_info$batch == "A", 5, 0)
    expr_mat[i, ] <- rnorm(n_samples, mean = 10 + batch_effect, sd = 1)
  }

  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )

  # Expect correction with default threshold (0.3)
  # Should see cli output about performing correction
  expect_snapshot(result <- auto_correct_batch_effect(exp, prop_threshold = 0.3))

  # Result should be different (corrected)
  expect_false(identical(result$expr_mat, exp$expr_mat))

  # Check that batch effects are reduced
  p_vals_orig <- suppressMessages(detect_batch_effect(exp))
  p_vals_corr <- suppressMessages(detect_batch_effect(result))

  # The proportion of significant variables should decrease after correction
  # p_vals_orig has high prop of significant vars (p < 0.05)
  # p_vals_corr should have lower prop of significant vars
  expect_lt(mean(p_vals_corr < 0.05), mean(p_vals_orig < 0.05))
})

test_that("auto_correct_batch_effect skips correction when below threshold", {
  set.seed(456)

  # Create experiment with NO batch effects
  n_vars <- 20
  n_samples <- 12
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:n_samples),
    batch = rep(c("A", "B"), each = 6)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))

  expr_mat <- matrix(rnorm(n_vars * n_samples, mean = 10, sd = 1), nrow = n_vars, ncol = n_samples)
  rownames(expr_mat) <- var_info$variable
  colnames(expr_mat) <- sample_info$sample

  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )

  # Expect NO correction
  expect_snapshot(result <- auto_correct_batch_effect(exp, prop_threshold = 0.3))

  # Result should be identical to original
  expect_identical(result$expr_mat, exp$expr_mat)
})

test_that("auto_correct_batch_effect handles missing batch column", {
  exp <- glyexp::toy_experiment
  # Remove batch column if exists
  exp$sample_info$batch <- NULL

  expect_snapshot(result <- auto_correct_batch_effect(exp, batch_col = "batch"))

  expect_identical(result, exp)
})

test_that("auto_correct_batch_effect uses group information", {
  set.seed(789)

  # Create experiment where batch and group are somewhat correlated but not fully confounded
  # This tests if the function passes group_col correctly

  n_vars <- 10
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    batch = rep(c("A", "B", "C"), each = 4),
    group = rep(c("Ctrl", "Treat"), 6)
  )
  var_info <- tibble::tibble(variable = paste0("V", 1:n_vars))
  # Use positive mean to ensure log2 works in ComBat
  expr_mat <- matrix(rnorm(120, mean = 10, sd = 2), nrow = 10, ncol = 12)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable

  exp <- glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "others"
  )

  # We just want to ensure it runs without error when group is provided
  # and that it's actually using the group column (detected by not erroring on a valid column)

  expect_snapshot(auto_correct_batch_effect(exp, group_col = "group", prop_threshold = 0))
})

test_that("auto_correct_batch_effect validation", {
  expect_error(auto_correct_batch_effect("invalid"), "Must inherit from class 'glyexp_experiment'")

  exp <- glyexp::toy_experiment
  expect_error(auto_correct_batch_effect(exp, prop_threshold = 1.5), "not <= 1")
})
