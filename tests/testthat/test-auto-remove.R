test_that("auto_remove works with simple preset", {
  # Setup: 10 vars, 10 samples (4A, 4B, 2QC)
  # QC samples should be ignored
  samples <- c(paste0("A", 1:4), paste0("B", 1:4), "QC1", "QC2")
  groups <- c(rep("A", 4), rep("B", 4), rep("QC", 2))
  
  expr_mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(expr_mat) <- samples
  rownames(expr_mat) <- paste0("V", 1:10)
  
  sample_info <- tibble::tibble(sample = samples, group = groups)
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  
  exp <- glyexp::experiment(expr_mat, sample_info, var_info)
  
  # V1: 60% missing in non-QC (5/8 samples: A1-A4, B1)
  exp$expr_mat["V1", c("A1", "A2", "A3", "A4", "B1")] <- NA
  
  # Run auto_remove
  expect_snapshot(res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC"))
  
  expect_false("V1" %in% rownames(res$expr_mat))
  expect_equal(nrow(res$expr_mat), 9)
})

test_that("auto_remove works with discovery preset", {
  samples <- c(paste0("A", 1:4), paste0("B", 1:4), "QC1", "QC2")
  groups <- c(rep("A", 4), rep("B", 4), rep("QC", 2))
  
  expr_mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(expr_mat) <- samples
  rownames(expr_mat) <- paste0("V", 1:10)
  
  sample_info <- tibble::tibble(sample = samples, group = groups)
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  
  exp <- glyexp::experiment(expr_mat, sample_info, var_info)
  
  # V1: 90% missing in non-QC (should be removed by global > 80%)
  exp$expr_mat["V1", c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4")] <- NA
  
  # V2: 60% missing in non-QC. Group A: 100% (>50%), Group B: 25% (<50%).
  # Should be KEPT (strict=FALSE)
  exp$expr_mat["V2", c("A1", "A2", "A3", "A4", "B1")] <- NA
  
  # V3: 60% missing in non-QC. Group A: 75% (>50%), Group B: 75% (>50%).
  # Should be REMOVED
  exp$expr_mat["V3", c("A1", "A2", "A3", "B1", "B2", "B3")] <- NA
  
  expect_snapshot(res <- auto_remove(exp, preset = "discovery", group_col = "group", qc_name = "QC"))
  
  expect_false("V1" %in% rownames(res$expr_mat))
  expect_true("V2" %in% rownames(res$expr_mat))
  expect_false("V3" %in% rownames(res$expr_mat))
})

test_that("auto_remove works with biomarker preset", {
  samples <- c(paste0("A", 1:4), paste0("B", 1:4), "QC1", "QC2")
  groups <- c(rep("A", 4), rep("B", 4), rep("QC", 2))
  
  expr_mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(expr_mat) <- samples
  rownames(expr_mat) <- paste0("V", 1:10)
  
  sample_info <- tibble::tibble(sample = samples, group = groups)
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  
  exp <- glyexp::experiment(expr_mat, sample_info, var_info)
  
  # V1: 50% missing (should be removed by global > 40%)
  exp$expr_mat["V1", c("A1", "A2", "A3", "A4")] <- NA
  
  # V2: 30% missing. Group A: 50% (<60%), Group B: 0%.
  # Should be KEPT
  exp$expr_mat["V2", c("A1", "A2")] <- NA
  
  # V3: 30% missing. Group A: 75% (>60%), Group B: 0%.
  # Should be REMOVED (strict=TRUE)
  exp$expr_mat["V3", c("A1", "A2", "A3")] <- NA
  
  expect_snapshot(res <- auto_remove(exp, preset = "biomarker", group_col = "group", qc_name = "QC"))
  
  expect_false("V1" %in% rownames(res$expr_mat))
  expect_true("V2" %in% rownames(res$expr_mat))
  expect_false("V3" %in% rownames(res$expr_mat))
})

test_that("auto_remove handles no QC samples", {
  samples <- c(paste0("A", 1:4), paste0("B", 1:4))
  groups <- c(rep("A", 4), rep("B", 4))
  
  expr_mat <- matrix(rnorm(80), nrow = 10, ncol = 8)
  colnames(expr_mat) <- samples
  rownames(expr_mat) <- paste0("V", 1:10)
  
  sample_info <- tibble::tibble(sample = samples, group = groups)
  var_info <- tibble::tibble(variable = paste0("V", 1:10))
  
  exp <- glyexp::experiment(expr_mat, sample_info, var_info)
  
  # V1: 62.5% missing (5/8)
  exp$expr_mat["V1", c("A1", "A2", "A3", "A4", "B1")] <- NA
  
  expect_snapshot(res <- auto_remove(exp, preset = "simple", group_col = "group", qc_name = "QC"))
  
  expect_false("V1" %in% rownames(res$expr_mat))
})

test_that("auto_remove errors with invalid input", {
  exp <- simple_exp(10, 5)
  expect_error(auto_remove(exp, preset = "invalid"), "Must be element of set")
})
