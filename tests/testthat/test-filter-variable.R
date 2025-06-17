test_that("filtering with proportion works", {
  exp <- simple_exp(5, 5)
  exp$expr_mat[1, 1] <- NA    # V1: 1/5 missing
  exp$expr_mat[2, 1:3] <- NA  # V2: 3/5 missing, should be removed

  res <- remove_missing_variables(exp, prop = 0.5)
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4", "V5"))
})


test_that("filtering with n works", {
  exp <- simple_exp(5, 5)
  exp$expr_mat[1, 1] <- NA    # V1: 1 missing
  exp$expr_mat[2, 1:3] <- NA  # V2: 3 missing, should be removed

  res <- remove_missing_variables(exp, n = 1)
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4", "V5"))
})


test_that("supplying both prop and n throws an error", {
  exp <- simple_exp(5, 5)
  expect_error(remove_missing_variables(exp, prop = 0.5, n = 1))
})


test_that("ignoring both prop and n uses prop = 0.5", {
  exp <- simple_exp(5, 5)
  exp$expr_mat[1, 1] <- NA    # V1: 1/5 missing
  exp$expr_mat[2, 1:3] <- NA  # V2: 3/5 missing, should be removed

  res <- remove_missing_variables(exp)
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4", "V5"))
})


test_that("filtering with by (strict = FALSE) works", {
  exp <- simple_exp(4, 6)
  exp$sample_info$group <- c("A", "A", "A", "B", "B", "B")
  # V1: missing in group A, should be kept
  exp$expr_mat[1, 1:3] <- NA
  # V2: 2 missings in both groups, should be removed
  exp$expr_mat[2, c(1, 2, 4, 5)] <- NA

  res <- remove_missing_variables(exp, by = "group", strict = FALSE)
  expect_equal(res$expr_mat, exp$expr_mat[-2, ])
  expect_equal(res$var_info$variable, c("V1", "V3", "V4"))
})


test_that("filtering with by (strict = TRUE) works", {
  exp <- simple_exp(4, 6)
  exp$sample_info$group <- c("A", "A", "A", "B", "B", "B")
  # V1: missing in group A, should be removed
  exp$expr_mat[1, 1:3] <- NA
  # V2: 2 missings in both groups, should be removed
  exp$expr_mat[2, c(1, 2, 4, 5)] <- NA

  res <- remove_missing_variables(exp, by = "group", strict = TRUE)
  expect_equal(res$expr_mat, exp$expr_mat[c(3, 4), ])
  expect_equal(res$var_info$variable, c("V3", "V4"))
})


# Test min_n functionality
test_that("default min_n calculation works correctly", {
  # Test with 2 samples (min_n should be 2)
  exp <- simple_exp(3, 2)
  exp$expr_mat[1, 1] <- NA    # V1: 1 non-missing (should be removed)
  exp$expr_mat[2, ] <- NA     # V2: 0 non-missing (should be removed)
  # V3: 2 non-missing (should be kept)

  res <- remove_missing_variables(exp, prop = 1)  # prop = 1 means no prop filtering
  expect_equal(res$var_info$variable, "V3")
  
  # Test with 4 samples (min_n should be 3)
  exp <- simple_exp(4, 4)
  exp$expr_mat[1, 1] <- NA    # V1: 3 non-missing (should be kept)
  exp$expr_mat[2, 1:2] <- NA  # V2: 2 non-missing (should be removed)
  exp$expr_mat[3, 1:3] <- NA  # V3: 1 non-missing (should be removed)
  # V4: 4 non-missing (should be kept)

  res <- remove_missing_variables(exp, prop = 1)  # prop = 1 means no prop filtering
  expect_equal(res$var_info$variable, c("V1", "V4"))
})


test_that("custom min_n works correctly", {
  exp <- simple_exp(4, 5)
  exp$expr_mat[1, 1] <- NA    # V1: 4 non-missing
  exp$expr_mat[2, 1:2] <- NA  # V2: 3 non-missing
  exp$expr_mat[3, 1:3] <- NA  # V3: 2 non-missing
  # V4: 5 non-missing

  # With min_n = 4, only V1 and V4 should be kept
  res <- remove_missing_variables(exp, prop = 1, min_n = 4)
  expect_equal(res$var_info$variable, c("V1", "V4"))
})


test_that("min_n validation works", {
  exp <- simple_exp(3, 4)
  
  # min_n greater than sample count should throw error
  expect_error(remove_missing_variables(exp, min_n = 5))
})


test_that("min_n with grouping works correctly", {
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
  res <- remove_missing_variables(exp, by = "group", prop = 1, strict = FALSE)
  expect_equal(res$var_info$variable, c("V1", "V3", "V4"))
  
  # With strict = TRUE, remove if fails in any group
  # V1: fails in A -> remove
  # V2: fails in both -> remove  
  # V3: fails in A -> remove
  # V4: passes in both -> keep
  res <- remove_missing_variables(exp, by = "group", prop = 1, strict = TRUE)
  expect_equal(res$var_info$variable, "V4")
})


test_that("min_n validation with grouping works", {
  exp <- simple_exp(3, 6)
  exp$sample_info$group <- c("A", "A", "B", "B", "B", "B")  # Group A: 2 samples, Group B: 4 samples
  
  # min_n = 3 should fail for group A (only 2 samples)
  expect_error(remove_missing_variables(exp, by = "group", min_n = 3))
})


test_that("min_n overrides prop filtering", {
  exp <- simple_exp(3, 4)
  exp$expr_mat[1, 1] <- NA    # V1: 3 non-missing, 25% missing
  exp$expr_mat[2, 1:2] <- NA  # V2: 2 non-missing, 50% missing
  # V3: 4 non-missing, 0% missing

  # With prop = 0.5 alone, V1 and V3 would be kept
  # But with min_n = 4, only V3 should be kept
  res <- remove_missing_variables(exp, prop = 0.5, min_n = 4)
  expect_equal(res$var_info$variable, "V3")
})
