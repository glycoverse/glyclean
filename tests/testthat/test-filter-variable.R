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
