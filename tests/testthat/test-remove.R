test_that("remove_rare: filtering with proportion works", {
  exp <- simple_exp(5, 5)
  exp$expr_mat[1, 1] <- NA # V1: 1/5 missing
  exp$expr_mat[2, 1:3] <- NA # V2: 3/5 missing, should be removed

  suppressMessages(res <- remove_rare(exp, prop = 0.5))
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4", "V5"))
})


test_that("remove_rare: filtering with n works", {
  exp <- simple_exp(5, 5)
  exp$expr_mat[1, 1] <- NA # V1: 1 missing
  exp$expr_mat[2, 1:3] <- NA # V2: 3 missing, should be removed

  suppressMessages(res <- remove_rare(exp, n = 1))
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4", "V5"))
})


test_that("remove_rare: supplying both prop and n throws an error", {
  exp <- simple_exp(5, 5)
  expect_error(remove_rare(exp, prop = 0.5, n = 1))
})


test_that("remove_rare: ignoring both prop and n uses prop = 0.5", {
  exp <- simple_exp(5, 5)
  exp$expr_mat[1, 1] <- NA # V1: 1/5 missing
  exp$expr_mat[2, 1:3] <- NA # V2: 3/5 missing, should be removed

  suppressMessages(res <- remove_rare(exp))
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4", "V5"))
})


test_that("remove_rare: filtering with by (strict = FALSE) works", {
  exp <- simple_exp(4, 6)
  exp$sample_info$group <- c("A", "A", "A", "B", "B", "B")
  # V1: missing in group A, should be kept
  exp$expr_mat[1, 1:3] <- NA
  # V2: 2 missings in both groups, should be removed
  exp$expr_mat[2, c(1, 2, 4, 5)] <- NA

  suppressMessages(res <- remove_rare(exp, by = "group", strict = FALSE))
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4"))
})


test_that("remove_rare: filtering with by (strict = TRUE) works", {
  exp <- simple_exp(4, 6)
  exp$sample_info$group <- c("A", "A", "A", "B", "B", "B")
  # V1: missing in group A, should be removed
  exp$expr_mat[1, 1:3] <- NA
  # V2: 2 missings in both groups, should be removed
  exp$expr_mat[2, c(1, 2, 4, 5)] <- NA

  suppressMessages(res <- remove_rare(exp, by = "group", strict = TRUE))
  expect_equal(res$expr_mat, exp$expr_mat[c(3, 4), ])
  expect_equal(res$var_info$variable, c("V3", "V4"))
})


# Test min_n functionality
test_that("remove_rare: default min_n calculation works correctly", {
  # Test with 2 samples (min_n should be 2)
  exp <- simple_exp(3, 2)
  exp$expr_mat[1, 1] <- NA # V1: 1 non-missing (should be removed)
  exp$expr_mat[2, ] <- NA # V2: 0 non-missing (should be removed)
  # V3: 2 non-missing (should be kept)

  suppressMessages(res <- remove_rare(exp, prop = 1)) # prop = 1 means no prop filtering
  expect_equal(res$var_info$variable, "V3")

  # Test with 4 samples (min_n should be 3)
  exp <- simple_exp(4, 4)
  exp$expr_mat[1, 1] <- NA # V1: 3 non-missing (should be kept)
  exp$expr_mat[2, 1:2] <- NA # V2: 2 non-missing (should be removed)
  exp$expr_mat[3, 1:3] <- NA # V3: 1 non-missing (should be removed)
  # V4: 4 non-missing (should be kept)

  suppressMessages(res <- remove_rare(exp, prop = 1)) # prop = 1 means no prop filtering
  expect_equal(res$var_info$variable, c("V1", "V4"))
})


test_that("remove_rare: custom min_n works correctly", {
  exp <- simple_exp(4, 5)
  exp$expr_mat[1, 1] <- NA # V1: 4 non-missing
  exp$expr_mat[2, 1:2] <- NA # V2: 3 non-missing
  exp$expr_mat[3, 1:3] <- NA # V3: 2 non-missing
  # V4: 5 non-missing

  # With min_n = 4, only V1 and V4 should be kept
  suppressMessages(res <- remove_rare(exp, prop = 1, min_n = 4))
  expect_equal(res$var_info$variable, c("V1", "V4"))
})


test_that("remove_rare: min_n validation works", {
  exp <- simple_exp(3, 4)

  # min_n greater than sample count should throw error
  expect_error(remove_rare(exp, min_n = 5))
})


test_that("remove_rare: min_n with grouping works correctly", {
  exp <- simple_exp(4, 6)
  exp$sample_info$group <- c("A", "A", "A", "B", "B", "B")

  # V1: 2 non-missing in A, 3 non-missing in B
  exp$expr_mat[1, 1] <- NA
  # V2: 1 non-missing in A, 2 non-missing in B
  exp$expr_mat[2, 1:2] <- NA
  exp$expr_mat[2, 4] <- NA
  # V3: 0 non-missing in A, 3 non-missing in B
  exp$expr_mat[3, 1:3] <- NA
  # V4: 3 non-missing in A, 3 non-missing in B (no NAs)

  # With default min_n (3 for each group), strict = FALSE
  # V1: fails in A (2 < 3), passes in B (3 >= 3) -> keep
  # V2: fails in A (1 < 3), fails in B (2 < 3) -> remove
  # V3: fails in A (0 < 3), passes in B (3 >= 3) -> keep
  # V4: passes in both -> keep
  suppressMessages(res <- remove_rare(exp, by = "group", prop = 1, strict = FALSE))
  expect_equal(res$var_info$variable, c("V1", "V3", "V4"))

  # With strict = TRUE, remove if fails in any group
  # V1: fails in A -> remove
  # V2: fails in both -> remove
  # V3: fails in A -> remove
  # V4: passes in both -> keep
  suppressMessages(res <- remove_rare(exp, by = "group", prop = 1, strict = TRUE))
  expect_equal(res$var_info$variable, "V4")
})


test_that("remove_rare: min_n validation with grouping works", {
  exp <- simple_exp(3, 6)
  exp$sample_info$group <- c("A", "A", "B", "B", "B", "B") # Group A: 2 samples, Group B: 4 samples

  # min_n = 3 should fail for group A (only 2 samples)
  expect_error(remove_rare(exp, by = "group", min_n = 3))
})


test_that("remove_rare: min_n overrides prop filtering", {
  exp <- simple_exp(3, 4)
  exp$expr_mat[1, 1] <- NA # V1: 3 non-missing, 25% missing
  exp$expr_mat[2, 1:2] <- NA # V2: 2 non-missing, 50% missing
  # V3: 4 non-missing, 0% missing

  # With prop = 0.5 alone, V1 and V3 would be kept
  # But with min_n = 4, only V3 should be kept
  suppressMessages(res <- remove_rare(exp, prop = 0.5, min_n = 4))
  expect_equal(res$var_info$variable, "V3")
})

test_that("remove_rare works with factor by parameter", {
  # Create test experiment
  test_exp <- complex_exp()

  # Introduce some missing values
  test_exp$expr_mat[1, 1] <- NA
  test_exp$expr_mat[2, 2] <- NA
  test_exp$expr_mat[3, 5] <- NA

  # Create a factor for grouping (matching the complex_exp structure)
  group_factor <- factor(test_exp$sample_info$group)

  # Apply filtering with factor by parameter
  suppressMessages(result_exp <- remove_rare(test_exp, by = group_factor, prop = 0.5, strict = FALSE))

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_true(nrow(result_exp$expr_mat) <= nrow(test_exp$expr_mat))
})

test_that("remove_rare works with character vector by parameter", {
  # Create test experiment
  test_exp <- complex_exp()

  # Introduce some missing values
  test_exp$expr_mat[1, 1:3] <- NA # More missing values in one group

  # Create a character vector for grouping
  group_vector <- rep(c("GroupA", "GroupB"), length.out = ncol(test_exp$expr_mat))

  # Apply filtering with vector by parameter
  suppressMessages(result_exp <- remove_rare(test_exp, by = group_vector, prop = 0.5, strict = FALSE))

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_true(nrow(result_exp$expr_mat) <= nrow(test_exp$expr_mat))
})

test_that("remove_rare by parameter validation works", {
  test_exp <- complex_exp()

  # Test with wrong length vector
  wrong_length_vector <- c("A", "B") # Should match number of samples
  expect_error(
    remove_rare(test_exp, by = wrong_length_vector, prop = 0.5),
    "vector must have length"
  )

  # Test with correct length numeric vector
  numeric_vector <- rep(c(1, 2), length.out = ncol(test_exp$expr_mat))
  suppressMessages(result_exp <- remove_rare(test_exp, by = numeric_vector, prop = 0.5))
  expect_s3_class(result_exp, "glyexp_experiment")
})

test_that("remove_rare: column name vs factor comparison", {
  # Create experiment with group column and missing values
  test_exp <- complex_exp()
  test_exp$expr_mat[1, 1:2] <- NA
  test_exp$expr_mat[2, 4:6] <- NA

  # Filter using column name
  suppressMessages(result_by_name <- remove_rare(test_exp, by = "group", prop = 0.5, strict = FALSE))

  # Filter using factor directly
  group_factor <- factor(test_exp$sample_info$group)
  suppressMessages(result_by_factor <- remove_rare(test_exp, by = group_factor, prop = 0.5, strict = FALSE))

  # Results should be identical
  expect_equal(result_by_name$expr_mat, result_by_factor$expr_mat)
  expect_equal(result_by_name$var_info, result_by_factor$var_info)
})

test_that("remove_rare works with matrix input", {
  # Create test matrix with missing values (5 samples to avoid min_n constraint)
  test_mat <- matrix(c(
    1, 2, 3, 4, 5, # V1: 0 missing
    6, NA, 8, 9, 10, # V2: 1 missing (20%)
    11, NA, NA, NA, 15, # V3: 3 missing (60%) - should be removed with prop = 0.5
    16, 17, 18, 19, 20 # V4: 0 missing
  ), nrow = 4, ncol = 5, byrow = TRUE)
  rownames(test_mat) <- paste0("V", 1:4)
  colnames(test_mat) <- paste0("S", 1:5)

  # Apply filtering
  suppressMessages(result_mat <- remove_rare(test_mat, prop = 0.5))

  # Check that the function returns a matrix
  expect_true(is.matrix(result_mat))
  expect_equal(nrow(result_mat), 3) # V3 should be removed
  expect_equal(ncol(result_mat), ncol(test_mat))
  expect_equal(rownames(result_mat), c("V1", "V2", "V4"))
  expect_equal(colnames(result_mat), colnames(test_mat))
})

test_that("remove_rare works with matrix input and by parameter", {
  # Create test matrix with missing values
  test_mat <- matrix(c(
    1, NA, 3, 4, 5, # V1: 1 missing in group A, 0 missing in group B
    NA, NA, 8, 9, 10, # V2: 2 missing in group A, 0 missing in group B
    11, 12, NA, NA, 15 # V3: 0 missing in group A, 2 missing in group B
  ), nrow = 3, ncol = 5, byrow = TRUE)
  rownames(test_mat) <- paste0("V", 1:3)
  colnames(test_mat) <- paste0("S", 1:5)

  # Create grouping vector
  by_vector <- c("A", "A", "B", "B", "B")

  # Apply filtering with by parameter
  suppressMessages(result_mat <- remove_rare(test_mat, by = by_vector, prop = 0.5, strict = FALSE))

  # Check that the function returns a matrix
  expect_true(is.matrix(result_mat))
  expect_equal(ncol(result_mat), ncol(test_mat))
  expect_equal(colnames(result_mat), colnames(test_mat))
})

test_that("matrix input with by parameter: column name should error", {
  # Create test matrix
  test_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)

  # Should error when column name is provided for matrix input
  expect_error(
    remove_rare(test_mat, by = "group", prop = 0.5),
    "For matrix input.*must be a vector, not a column name"
  )
})

test_that("matrix input with by parameter: wrong length vector should error", {
  # Create test matrix
  test_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)

  # Should error when vector length doesn't match sample count
  expect_error(
    remove_rare(test_mat, by = c("A", "B"), prop = 0.5),
    "vector must have length 3"
  )
})

# Additional tests to improve coverage

test_that("remove_rare with matrix input and small sample sizes", {
  # Test with 1 sample (min_n should be 1)
  mat1 <- matrix(c(1, NA, 3), nrow = 3, ncol = 1)
  rownames(mat1) <- paste0("V", 1:3)
  colnames(mat1) <- "S1"

  suppressMessages(result <- remove_rare(mat1, prop = 1)) # No prop filtering
  expect_equal(nrow(result), 2) # V2 should be removed due to min_n

  # Test with 2 samples (min_n should be 2)
  mat2 <- matrix(c(1, NA, 3, 4, NA, 6), nrow = 3, ncol = 2)
  rownames(mat2) <- paste0("V", 1:3)
  colnames(mat2) <- paste0("S", 1:2)

  suppressMessages(result <- remove_rare(mat2, prop = 1))
  expect_equal(nrow(result), 2) # V2 should be removed due to min_n

  # Test with 3 samples (min_n should be 3)
  mat3 <- matrix(c(1, NA, 3, 4, NA, 6, 7, NA, 9), nrow = 3, ncol = 3)
  rownames(mat3) <- paste0("V", 1:3)
  colnames(mat3) <- paste0("S", 1:3)

  suppressMessages(result <- remove_rare(mat3, prop = 1))
  expect_equal(nrow(result), 2) # V2 should be removed due to min_n
})

test_that("remove_rare with matrix input and grouping small sample sizes", {
  # Test with small groups
  mat <- matrix(c(1, NA, 3, 4, NA, 6), nrow = 2, ncol = 3)
  rownames(mat) <- paste0("V", 1:2)
  colnames(mat) <- paste0("S", 1:3)

  # Group A: 1 sample, Group B: 2 samples
  by_vector <- c("A", "B", "B")

  # Test with default min_n calculation
  suppressMessages(result <- remove_rare(mat, by = by_vector, prop = 1, strict = FALSE))
  expect_true(is.matrix(result))

  # Test with strict = TRUE
  suppressMessages(result <- remove_rare(mat, by = by_vector, prop = 1, strict = TRUE))
  expect_true(is.matrix(result))
})

test_that("remove_rare handles edge cases with all missing or no missing", {
  # Test matrix with variable that has high missing rate (use 5 samples to avoid min_n constraint)
  mat <- matrix(c(
    1, 2, 3, 4, 5, # V1: 0 missing
    NA, NA, NA, 4, 5 # V2: 3 missing (60% > 50%)
  ), nrow = 2, ncol = 5, byrow = TRUE)
  rownames(mat) <- paste0("V", 1:2)
  colnames(mat) <- paste0("S", 1:5)

  suppressMessages(result <- remove_rare(mat, prop = 0.5))
  expect_equal(nrow(result), 1) # V2 should be removed
  expect_equal(rownames(result), "V1")

  # Test with no missing values
  mat_no_na <- matrix(1:10, nrow = 2, ncol = 5)
  rownames(mat_no_na) <- paste0("V", 1:2)
  colnames(mat_no_na) <- paste0("S", 1:5)

  suppressMessages(result <- remove_rare(mat_no_na, prop = 0.5))
  expect_equal(dim(result), dim(mat_no_na))
})

test_that("remove_rare parameter validation edge cases", {
  exp <- simple_exp(3, 4)

  # Test prop = 0 (no missing values allowed)
  suppressMessages(result <- remove_rare(exp, prop = 0))
  expect_s3_class(result, "glyexp_experiment")

  # Test prop = 1 (all missing values allowed)
  suppressMessages(result <- remove_rare(exp, prop = 1))
  expect_s3_class(result, "glyexp_experiment")

  # Test n = 0 (no missing values allowed)
  suppressMessages(result <- remove_rare(exp, n = 0))
  expect_s3_class(result, "glyexp_experiment")

  # Test with min_n = 1
  suppressMessages(result <- remove_rare(exp, min_n = 1))
  expect_s3_class(result, "glyexp_experiment")
})

test_that("remove_rare with complex grouping scenarios", {
  # Create experiment with unbalanced groups
  exp <- simple_exp(5, 8)
  exp$sample_info$group <- c("A", "A", "B", "B", "B", "C", "C", "C")

  # Create specific missing patterns
  exp$expr_mat[1, c(1, 3, 6)] <- NA # Missing in different groups
  exp$expr_mat[2, c(1, 2)] <- NA # Missing only in group A
  exp$expr_mat[3, c(3, 4, 5)] <- NA # Missing only in group B
  exp$expr_mat[4, c(6, 7, 8)] <- NA # Missing only in group C

  # Test with various combinations
  suppressMessages(result1 <- remove_rare(exp, by = "group", prop = 0.4, strict = FALSE))
  expect_s3_class(result1, "glyexp_experiment")

  suppressMessages(result2 <- remove_rare(exp, by = "group", prop = 0.4, strict = TRUE))
  expect_s3_class(result2, "glyexp_experiment")

  suppressMessages(result3 <- remove_rare(exp, by = "group", n = 1, strict = FALSE))
  expect_s3_class(result3, "glyexp_experiment")
})

test_that("remove_rare with matrix and various invalid inputs", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)

  # Test with invalid prop values
  expect_error(remove_rare(mat, prop = -0.1))
  expect_error(remove_rare(mat, prop = 1.1))

  # Test with invalid n values
  expect_error(remove_rare(mat, n = -1))

  # Test with invalid min_n values
  expect_error(remove_rare(mat, min_n = 0))
  expect_error(remove_rare(mat, min_n = -1))
})

test_that("remove_low_var works with matrix input", {
  mat <- matrix(1:9, nrow = 3, byrow = TRUE)
  rownames(mat) <- paste0("V", 1:3)
  colnames(mat) <- paste0("S", 1:3)
  mat[1, ] <- 1 # the first variable has zero variance

  suppressMessages(result <- remove_low_var(mat))
  expect_equal(rownames(result), c("V2", "V3"))
})

test_that("remove_low_var works with experiment input", {
  exp <- simple_exp(3, 3)
  exp$expr_mat[1, ] <- 1 # the first variable has zero variance

  suppressMessages(result <- remove_low_var(exp))
  expected_mat <- exp$expr_mat[-1, ]
  expected_var_info <- exp$var_info[-1, ]
  expect_equal(result$expr_mat, expected_mat)
  expect_equal(result$var_info, expected_var_info)
})

test_that("remove_low_var respects var_cutoff", {
  mat <- matrix(c(5.0, 5.05, 4.95, 1, 2, 3), nrow = 2, byrow = TRUE)
  rownames(mat) <- paste0("V", 1:2)
  colnames(mat) <- paste0("S", 1:3)

  suppressMessages(result <- remove_low_var(mat, var_cutoff = 0.01))
  expect_equal(rownames(result), c("V2"))
})

test_that("remove_low_var works with grouping: strict = FALSE (delete only if ALL groups pass threshold)", {
  mat <- matrix(
    c(
      # V1: Group A all 5 (variance=0), Group B all 5 (variance=0) -> both groups low variance -> should be removed
      5, 5, 5, 5, 5, 5,
      # V2: Group A all 10 (variance=0), Group B 1,5,9 (variance>0) -> only Group A low variance -> should be kept in strict=FALSE
      10, 10, 10, 1, 5, 9,
      # V3: Both groups high variance -> should be kept
      1, 2, 3, 4, 5, 6,
      # V4: Both groups high variance -> should be kept
      0, 10, 20, 30, 40, 50
    ),
    nrow = 4, byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  colnames(mat) <- paste0("S", 1:6)
  by_fac <- factor(c("A", "A", "A", "B", "B", "B"))

  suppressMessages(res <- remove_low_var(mat, var_cutoff = 0, by = by_fac, strict = FALSE))
  # Only remove if all groups pass threshold -> V1 is removed, V2/V3/V4 are kept
  expect_equal(rownames(res), c("V2", "V3", "V4"))
})

test_that("remove_low_var works with grouping: strict = TRUE (delete if ANY group passes threshold)", {
  mat <- matrix(
    c(
      # V1: Group A zero variance, Group B zero variance -> any group meets threshold -> remove
      5, 5, 5, 5, 5, 5,
      # V2: Group A zero variance, Group B high variance -> any group meets threshold -> remove
      10, 10, 10, 1, 5, 9,
      # V3: Both groups high variance -> keep
      1, 2, 3, 4, 5, 6,
      # V4: Both groups high variance -> keep
      0, 10, 20, 30, 40, 50
    ),
    nrow = 4, byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  colnames(mat) <- paste0("S", 1:6)

  by_fac <- factor(c("A", "A", "A", "B", "B", "B"))

  suppressMessages(res <- remove_low_var(mat, var_cutoff = 0, by = by_fac, strict = TRUE))
  # Any group meets threshold -> V1 and V2 are removed, V3 and V4 are kept
  expect_equal(rownames(res), c("V3", "V4"))
})

test_that("remove_low_var grouping works for by types: factor / character / numeric", {
  mat <- matrix(
    c(
      5, 5, 5, 5, 5, 5, # V1: Both groups low variance -> remove regardless of by type (strict=TRUE)
      10, 10, 10, 1, 5, 9, # V2: Group A low variance, Group B high variance -> remove in strict=TRUE
      1, 2, 3, 4, 5, 6 # V3: Both groups high variance -> keep
    ),
    nrow = 3, byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:3)
  colnames(mat) <- paste0("S", 1:6)

  by_factor <- factor(c("A", "A", "A", "B", "B", "B"))
  by_char <- c("A", "A", "A", "B", "B", "B")
  by_num <- c(1, 1, 1, 2, 2, 2)

  suppressMessages(res_fac <- remove_low_var(mat, var_cutoff = 0, by = by_factor, strict = TRUE))
  suppressMessages(res_chr <- remove_low_var(mat, var_cutoff = 0, by = by_char, strict = TRUE))
  suppressMessages(res_num <- remove_low_var(mat, var_cutoff = 0, by = by_num, strict = TRUE))

  # Three by types should get the same result: only V3 is kept
  expect_equal(rownames(res_fac), c("V3"))
  expect_equal(rownames(res_chr), c("V3"))
  expect_equal(rownames(res_num), c("V3"))
})

test_that("remove_low_var errors when by length mismatches ncol(x)", {
  mat <- matrix(1:12, nrow = 3) # 3x4
  by_bad <- c("A", "A", "B") # less than 4

  expect_error(
    remove_low_var(mat, var_cutoff = 0, by = by_bad),
    regexp = "len|length|ncol", # tolerate different error messages
    fixed = FALSE
  )
})

test_that("remove_low_var errors when by contains NA for grouped summarization", {
  mat <- matrix(1:12, nrow = 3) # 3x4
  by_na <- c("A", "A", NA, "B")

  expect_error(
    remove_low_var(mat, var_cutoff = 0, by = by_na),
    regexp = "must not contain NA|NA", # match cli_abort in implementation
    fixed = FALSE
  )
})

test_that("remove_low_var handles rows with some NAs", {
  # V1: contains NA but zero variance across non-NA values -> should be removed when var_cutoff = 0
  # V2: contains NA but non-zero variance -> should be retained
  # V3: normal row, retained
  mat <- matrix(
    c(
      1, 1, NA, # V1
      1, 2, NA, # V2
      3, 4, 5 # V3
    ),
    nrow = 3, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2", "V3")
  colnames(mat) <- c("S1", "S2", "S3")

  # Variance threshold: V1 has var=0 with na.rm=TRUE -> removed; V2/V3 stay.
  suppressMessages(res <- remove_low_var(mat, var_cutoff = 0))
  expect_equal(rownames(res), c("V2", "V3"))
})

test_that("remove_low_var keeps all-NA row and remains stable", {
  # V1: all NA -> var = NA (with na.rm=TRUE) -> treated as Inf by helper; should be retained
  # V2: normal varying row
  mat <- matrix(
    c(
      NA, NA, NA, # V1: all NA
      1,  2,  3 # V2
    ),
    nrow = 2, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2")
  colnames(mat) <- c("S1", "S2", "S3")

  suppressMessages(res <- remove_low_var(mat, var_cutoff = 0))
  expect_equal(rownames(res), c("V1", "V2"))
})

test_that("rows containing NaN or Inf do not error and produce predictable filtering", {
  # V1: includes NaN -> var with na.rm=TRUE should ignore NaN and use (1,2), var=0.5 -> not removed by small var_cutoff
  # V2: includes +Inf -> variance is Inf -> retained
  # V3: constant finite row -> removed by var_cutoff=0
  mat <- matrix(
    c(
      1,   NaN, 2, # V1
      100, 100, Inf, # V2
      5,   5,   5 # V3: zero variance
    ),
    nrow = 3, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2", "V3")
  colnames(mat) <- c("S1", "S2", "S3")

  # Use both thresholds (tiny), expect:
  # - V3 removed due to zero variance;
  # - V1 retained (var=0.5 > 0);
  # - V2 retained (var Inf), no errors thrown.
  suppressMessages(res <- remove_low_var(mat, var_cutoff = 0))
  expect_equal(rownames(res), c("V1", "V2"))
})

test_that("remove_low_var ignores unused levels of by", {
  mat <- matrix(1:12, nrow = 3)
  rownames(mat) <- c("V1", "V2", "V3")
  colnames(mat) <- c("S1", "S2", "S3", "S4")
  by <- factor(c("A", "A", "B", "B"), levels = c("A", "B", "C"))
  suppressMessages(res <- remove_low_var(mat, var_cutoff = 0, by = by))
  expect_equal(rownames(res), c("V1", "V2", "V3"))
})

test_that("remove_low_var works with one-row matrix", {
  mat <- matrix(1:4, nrow = 1)
  rownames(mat) <- c("V1")
  suppressMessages(res <- remove_low_var(mat))
  expect_equal(rownames(res), c("V1"))
})

test_that("remove_low_var works with one-column matrix", {
  mat <- matrix(1:4, ncol = 1)
  rownames(mat) <- c("V1", "V2", "V3", "V4")
  suppressMessages(res <- remove_low_var(mat))
  expect_equal(rownames(res), c("V1", "V2", "V3", "V4"))
})

test_that("remove_low_cv works with matrix input", {
  mat <- matrix(
    c(
      1, 1, 1, # V1: zero CV
      1, 2, 3, # V2: non-zero CV
      4, 5, 6 # V3: non-zero CV
    ),
    nrow = 3, byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:3)
  colnames(mat) <- paste0("S", 1:3)

  suppressMessages(result <- remove_low_cv(mat))
  expect_equal(rownames(result), c("V2", "V3"))
})

test_that("remove_low_cv works with experiment input", {
  exp <- simple_exp(3, 3)
  exp$expr_mat[1, ] <- 1 # zero CV

  suppressMessages(result <- remove_low_cv(exp))
  expected_mat <- exp$expr_mat[-1, ]
  expected_var_info <- exp$var_info[-1, ]
  expect_equal(result$expr_mat, expected_mat)
  expect_equal(result$var_info, expected_var_info)
})

test_that("remove_low_cv respects cv_cutoff", {
  mat <- matrix(c(100, 101, 99, 1, 2, 3), nrow = 2, byrow = TRUE)
  rownames(mat) <- paste0("V", 1:2)
  colnames(mat) <- paste0("S", 1:3)

  suppressMessages(result <- remove_low_cv(mat, cv_cutoff = 0.1))
  expect_equal(rownames(result), c("V2"))
})

test_that("remove_low_cv works with grouping: strict = FALSE", {
  mat <- matrix(
    c(
      5, 5, 5, 5, 5, 5, # V1: zero CV in both groups -> remove
      10, 10, 10, 1, 5, 9, # V2: zero CV in group A only -> keep when strict = FALSE
      1, 2, 3, 4, 5, 6, # V3: non-zero CV in both groups -> keep
      0, 10, 20, 30, 40, 50 # V4: non-zero CV in both groups -> keep
    ),
    nrow = 4, byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  colnames(mat) <- paste0("S", 1:6)
  by_fac <- factor(c("A", "A", "A", "B", "B", "B"))

  suppressMessages(res <- remove_low_cv(mat, cv_cutoff = 0, by = by_fac, strict = FALSE))
  expect_equal(rownames(res), c("V2", "V3", "V4"))
})

test_that("remove_low_cv works with grouping: strict = TRUE", {
  mat <- matrix(
    c(
      5, 5, 5, 5, 5, 5,
      10, 10, 10, 1, 5, 9,
      1, 2, 3, 4, 5, 6,
      0, 10, 20, 30, 40, 50
    ),
    nrow = 4, byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  colnames(mat) <- paste0("S", 1:6)
  by_fac <- factor(c("A", "A", "A", "B", "B", "B"))

  suppressMessages(res <- remove_low_cv(mat, cv_cutoff = 0, by = by_fac, strict = TRUE))
  expect_equal(rownames(res), c("V3", "V4"))
})

test_that("remove_low_cv handles rows with some NAs", {
  mat <- matrix(
    c(
      1, 1, NA, # V1: zero CV across observed values -> remove
      1, 2, NA, # V2: non-zero CV -> keep
      3, 4, 5 # V3: non-zero CV -> keep
    ),
    nrow = 3, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2", "V3")
  colnames(mat) <- c("S1", "S2", "S3")

  suppressMessages(res <- remove_low_cv(mat, cv_cutoff = 0))
  expect_equal(rownames(res), c("V2", "V3"))
})

test_that("remove_low_cv keeps all-NA row and remains stable", {
  mat <- matrix(
    c(
      NA, NA, NA, # V1: all NA -> CV helper returns Inf -> keep
      1,  2,  3 # V2: finite CV -> depends on cutoff
    ),
    nrow = 2, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2")
  colnames(mat) <- c("S1", "S2", "S3")

  suppressMessages(res <- remove_low_cv(mat, cv_cutoff = 0))
  expect_equal(rownames(res), c("V1", "V2"))
})

test_that("remove_low_cv keeps zero-mean row because CV is infinite", {
  mat <- matrix(
    c(
      -1, 0, 1, # V1: mean 0 -> CV = Inf -> keep
      10, 12, 14 # V2: finite CV -> may be filtered
    ),
    nrow = 2, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2")
  colnames(mat) <- c("S1", "S2", "S3")

  suppressMessages(res <- remove_low_cv(mat, cv_cutoff = 0.5))
  expect_true("V1" %in% rownames(res))
})

test_that("remove_low_cv removes low CV row even with negative mean", {
  mat <- matrix(
    c(
      -10, -11, -9, # V1: CV ~ 0.1 -> remove with cutoff 0.2
      10,  14,  6 # V2: higher CV -> keep
    ),
    nrow = 2, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2")
  colnames(mat) <- c("S1", "S2", "S3")

  suppressMessages(res <- remove_low_cv(mat, cv_cutoff = 0.2))
  expect_equal(rownames(res), "V2")
})

test_that("remove_low_cv handles NaN and Inf gracefully", {
  mat <- matrix(
    c(
      1,   NaN, 2, # V1: finite CV -> keep
      100, 100, Inf, # V2: CV = Inf -> keep
      5,   5,   5 # V3: CV = 0 -> remove
    ),
    nrow = 3, byrow = TRUE
  )
  rownames(mat) <- c("V1", "V2", "V3")
  colnames(mat) <- c("S1", "S2", "S3")

  suppressMessages(res <- remove_low_cv(mat, cv_cutoff = 0))
  expect_equal(rownames(res), c("V1", "V2"))
})

test_that("remove_constant works", {
  mat <- matrix(1:9, nrow = 3)
  rownames(mat) <- c("V1", "V2", "V3")
  colnames(mat) <- c("S1", "S2", "S3")
  mat[1, ] <- 1
  suppressMessages(res <- remove_constant(mat))
  expect_equal(rownames(res), c("V2", "V3"))
})

test_that("remove_low_expr matrix input retains matrix type and dimnames", {
  mat <- matrix(
    c(
      1, 3, 5,
      2, 4, 6,
      10, 12, 14,
      11, 13, 15
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(mat) <- paste0("r", 1:4)
  colnames(mat) <- paste0("c", 1:3)

  suppressMessages(res <- remove_low_expr(mat, percentile = 0.5))

  expect_true(is.matrix(res))
  expect_identical(colnames(res), colnames(mat))
  expect_identical(rownames(res), c("r3", "r4"))
})

test_that("remove_low_expr filters glyexp_experiment while preserving class", {
  exp <- simple_exp(4, 4)
  exp$expr_mat[1, ] <- c(0, 1, 2, 3)
  exp$expr_mat[2, ] <- c(2, 2, 2, 2)
  exp$expr_mat[3, ] <- c(5, 5, 5, 5)
  exp$expr_mat[4, ] <- c(9, 9, 9, 9)

  suppressMessages(res <- remove_low_expr(exp, percentile = 0.5))

  expect_s3_class(res, "glyexp_experiment")
  expect_identical(rownames(res$expr_mat), c("V3", "V4"))
  expect_identical(res$var_info$variable, c("V3", "V4"))
})

test_that("remove_low_expr by column name or vector gives identical results", {
  exp <- simple_exp(4, 6)
  exp$sample_info$group <- rep(c("A", "B", "C"), each = 2)
  exp$expr_mat[1, ] <- c(1, 1, 9, 9, 9, 9)
  exp$expr_mat[2, ] <- c(2, 2, 2, 2, 2, 2)
  exp$expr_mat[3, ] <- c(3, 3, 3, 3, 3, 3)
  exp$expr_mat[4, ] <- c(10, 10, 10, 10, 10, 10)

  suppressMessages(res_by_name <- remove_low_expr(exp, percentile = 0.4, by = "group", strict = TRUE))
  suppressMessages(res_by_vec <- remove_low_expr(exp, percentile = 0.4, by = exp$sample_info$group, strict = TRUE))

  expect_identical(res_by_name$expr_mat, res_by_vec$expr_mat)
  expect_identical(res_by_name$var_info, res_by_vec$var_info)
})

test_that("remove_low_expr errors on non-numeric inputs", {
  char_mat <- matrix(letters[1:6], nrow = 2)
  expect_error(remove_low_expr(char_mat, percentile = 0.5), "numeric")

  df_input <- data.frame(char_mat, stringsAsFactors = FALSE)
  expect_error(remove_low_expr(df_input), "must be a .*matrix")
})

test_that("remove_low_expr validates percentile bounds", {
  mat <- matrix(1:9, nrow = 3)

  expect_error(remove_low_expr(mat, percentile = -0.01), "percentile")
  expect_error(remove_low_expr(mat, percentile = 1.01), "percentile")
})

test_that("remove_low_expr validates by length and NA content", {
  mat <- matrix(1:9, nrow = 3)

  expect_error(
    remove_low_expr(mat, percentile = 0.5, by = c("A", "B")),
    "length 3"
  )
  expect_error(
    remove_low_expr(mat, percentile = 0.5, by = c("A", NA, "B")),
    "`by` must not contain NA"
  )
})

test_that("remove_low_expr global filtering removes rows at or below threshold", {
  mat <- matrix(
    c(
      1, 1, 1,
      2, 2, 2,
      5, 5, 5
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:3)

  suppressMessages(res <- remove_low_expr(mat, percentile = 0.5))

  expect_identical(rownames(res), "V3")
})

test_that("remove_low_expr percentile extremes behave as expected", {
  mat <- matrix(
    c(
      0, 0, 0,
      0, 0, 1,
      5, 5, 5
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:3)
  colnames(mat) <- paste0("S", 1:3)

  suppressMessages(res_min <- remove_low_expr(mat, percentile = 0))
  expect_identical(rownames(res_min), "V3")

  suppressMessages(res_max <- remove_low_expr(mat, percentile = 1))
  expect_true(is.matrix(res_max))
  expect_equal(nrow(res_max), 0)
  expect_identical(colnames(res_max), colnames(mat))
})

test_that("remove_low_expr grouped filtering uses per-group thresholds", {
  mat <- matrix(
    c(
      1, 1, 10, 10,
      2, 2, 2, 2,
      5, 5, 1, 1,
      9, 9, 9, 9
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  by <- c("A", "A", "B", "B")

  suppressMessages(res_group <- remove_low_expr(mat, percentile = 0.5, by = by, strict = FALSE))
  suppressMessages(res_global <- remove_low_expr(mat, percentile = 0.5))

  expect_identical(rownames(res_group), c("V1", "V3", "V4"))
  expect_identical(rownames(res_global), c("V1", "V4"))
})

test_that("remove_low_expr strict flag controls grouped filtering", {
  mat <- matrix(
    c(
      1, 1, 10, 10,
      2, 2, 2, 2,
      5, 5, 1, 1,
      9, 9, 9, 9
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  by <- c("A", "A", "B", "B")

  suppressMessages(res_all <- remove_low_expr(mat, percentile = 0.5, by = by, strict = FALSE))
  suppressMessages(res_any <- remove_low_expr(mat, percentile = 0.5, by = by, strict = TRUE))

  expect_identical(rownames(res_all), c("V1", "V3", "V4"))
  expect_identical(rownames(res_any), "V4")
})

test_that("remove_low_expr handles different by input types consistently", {
  mat <- matrix(
    c(
      1, 1, 10, 10,
      2, 2, 2, 2,
      5, 5, 1, 1,
      9, 9, 9, 9
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  by_factor <- factor(c("A", "A", "B", "B"))
  by_char <- c("A", "A", "B", "B")
  by_num <- c(1, 1, 2, 2)

  suppressMessages(res_factor <- remove_low_expr(mat, percentile = 0.4, by = by_factor, strict = TRUE))
  suppressMessages(res_char <- remove_low_expr(mat, percentile = 0.4, by = by_char, strict = TRUE))
  suppressMessages(res_num <- remove_low_expr(mat, percentile = 0.4, by = by_num, strict = TRUE))

  expect_identical(res_factor, res_char)
  expect_identical(res_factor, res_num)
})

test_that("remove_low_expr outcome is invariant to column order when by is aligned", {
  mat <- matrix(
    c(
      1, 1, 10, 10,
      2, 2, 2, 2,
      5, 5, 1, 1,
      9, 9, 9, 9
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)
  by <- c("A", "A", "B", "B")

  suppressMessages(baseline <- remove_low_expr(mat, percentile = 0.5, by = by, strict = FALSE))

  perm <- c(3, 4, 1, 2)
  mat_perm <- mat[, perm]
  by_perm <- by[perm]
  suppressMessages(res_perm <- remove_low_expr(mat_perm, percentile = 0.5, by = by_perm, strict = FALSE))

  expect_identical(rownames(baseline), rownames(res_perm))
  expect_identical(colnames(res_perm), colnames(mat_perm))
})

test_that("remove_low_expr tolerates empty grouping levels", {
  mat <- matrix(
    c(
      1, 1, 1, 1,
      9, 9, 9, 9
    ),
    nrow = 2,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:2)
  by <- factor(c("A", "A", "B", "B"), levels = c("A", "B", "C"))

  suppressMessages(res <- remove_low_expr(mat, percentile = 0.5, by = by, strict = TRUE))

  expect_identical(rownames(res), "V2")
})

test_that("remove_low_expr handles NA, NaN, and Inf without dropping unexpectedly", {
  mat <- matrix(
    c(
      NA, NA, NA,
      1, 2, NA,
      NaN, NaN, NaN,
      Inf, 100, 200
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:4)

  suppressMessages(res <- remove_low_expr(mat, percentile = 0.4))

  expect_true("V1" %in% rownames(res)) # all NA row kept
  expect_true("V3" %in% rownames(res)) # NaN-only row kept
  expect_true("V4" %in% rownames(res)) # Inf handled
})

test_that("remove_low_expr honours strict logic when some groups are entirely NA", {
  mat <- matrix(
    c(
      1, NA, 0, 0,
      0, NA, 0, 0,
      NA, NA, 5, 5
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(mat) <- paste0("V", 1:3)
  by <- c("A", "A", "B", "B")

  suppressMessages(res_all <- remove_low_expr(mat, percentile = 0.5, by = by, strict = FALSE))
  suppressMessages(res_any <- remove_low_expr(mat, percentile = 0.5, by = by, strict = TRUE))

  expect_identical(rownames(res_all), c("V1", "V3"))
  expect_identical(rownames(res_any), "V3")
})

test_that("remove_low_expr handles single row or single column matrices", {
  single_row <- matrix(c(1, 2, 3), nrow = 1)
  colnames(single_row) <- paste0("S", 1:3)
  suppressMessages(res_row <- remove_low_expr(single_row, percentile = 0.5))
  expect_true(is.matrix(res_row))
  expect_equal(nrow(res_row), 0)
  expect_identical(colnames(res_row), colnames(single_row))

  single_col <- matrix(c(1, 2, 3), nrow = 3)
  rownames(single_col) <- paste0("V", 1:3)
  suppressMessages(res_col <- remove_low_expr(single_col, percentile = 0.2))
  expect_true(is.matrix(res_col))
  expect_true(all(rownames(res_col) %in% rownames(single_col)))
})

test_that("remove_low_expr returns input unchanged when all medians are NA", {
  mat <- matrix(NA_real_, nrow = 3, ncol = 4)
  rownames(mat) <- paste0("V", 1:3)
  colnames(mat) <- paste0("S", 1:4)

  suppressMessages(res <- remove_low_expr(mat, percentile = 0.75))

  expect_identical(res, mat)
})

test_that("remove_low_expr is deterministic across repeated calls", {
  mat <- matrix(
    c(
      1, 1, 1, 1,
      2, 2, 2, 2,
      3, 3, 3, 3,
      10, 10, 10, 10
    ),
    nrow = 4,
    byrow = TRUE
  )
  by <- c("A", "A", "B", "B")

  suppressMessages(res1 <- remove_low_expr(mat, percentile = 0.4, by = by, strict = FALSE))
  suppressMessages(res2 <- remove_low_expr(mat, percentile = 0.4, by = by, strict = FALSE))

  expect_identical(res1, res2)
})
