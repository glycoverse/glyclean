test_that("transform_clr works with experiment input", {
  test_exp <- simple_exp(5, 5)
  test_exp$expr_mat <- matrix(
    c(
      4,
      4,
      4,
      4,
      4,
      16,
      16,
      16,
      16,
      16,
      64,
      64,
      64,
      64,
      64,
      8,
      8,
      8,
      8,
      8,
      32,
      32,
      32,
      32,
      32
    ),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(
      rownames(test_exp$expr_mat),
      colnames(test_exp$expr_mat)
    )
  )
  original_exp <- test_exp
  result_exp <- transform_clr(test_exp, gamma = 0)

  # Check that the function returns the correct structure
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(dim(result_exp$expr_mat), dim(original_exp$expr_mat))
  expect_equal(rownames(result_exp$expr_mat), rownames(original_exp$expr_mat))
  expect_equal(colnames(result_exp$expr_mat), colnames(original_exp$expr_mat))

  expect_equal(
    unname(result_exp$expr_mat[, 1]),
    c(0.25, 1, 4, 0.5, 2)
  )
})

test_that("transform_clr uses non-zero entries for the geometric mean", {
  test_mat <- matrix(
    c(
      4,
      0,
      16,
      4,
      64,
      0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      paste0("V", 1:3),
      paste0("S", 1:2)
    )
  )

  result_mat <- transform_clr(test_mat, gamma = 0)

  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(rownames(result_mat), rownames(test_mat))
  expect_equal(colnames(result_mat), colnames(test_mat))
  expect_equal(unname(result_mat[, "S1"]), c(0.25, 1, 4))
  expect_equal(unname(result_mat["V1", "S2"]), 0)
  expect_equal(unname(result_mat["V2", "S2"]), 1)
  expect_equal(unname(result_mat["V3", "S2"]), 0)
})

test_that("transform_clr samples per-feature noise on the log2 scale", {
  test_mat <- matrix(
    c(4, 16),
    nrow = 2,
    dimnames = list(c("V1", "V2"), "S1")
  )

  expected_noise <- withr::with_seed(1, stats::rnorm(2, mean = -3, sd = 0.1))
  result_mat <- withr::with_seed(1, transform_clr(test_mat, gamma = 0.1))

  expect_equal(unname(result_mat[, 1]), 2^(c(2, 4) + expected_noise))
})

test_that("transform_clr ignores group scales when gamma is zero", {
  test_mat <- matrix(
    c(
      4,
      4,
      4,
      4,
      16,
      16,
      16,
      16
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("V1", "V2"),
      paste0("S", 1:4)
    )
  )
  groups <- factor(c("A", "A", "B", "B"))

  result_mat <- transform_clr(
    test_mat,
    by = groups,
    group_scales = c(A = 1, B = 0.5),
    gamma = 0
  )

  expect_equal(unname(result_mat[, "S1"]), c(0.5, 2))
  expect_equal(unname(result_mat[, "S3"]), c(0.5, 2))
})

test_that("transform_clr errors on missing values and negative entries", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  expect_error(transform_clr(test_exp, gamma = 0), "missing")

  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- -1
  expect_error(transform_clr(test_exp, gamma = 0), "negative|positive")
})

test_that("transform_clr errors on unsupported input", {
  expect_error(transform_clr(1), "glyexp_experiment|matrix")
  expect_error(transform_clr("string"), "glyexp_experiment|matrix")
})

test_that("transform_alr works with experiment input", {
  test_exp <- simple_exp(5, 5)
  test_exp$sample_info$group <- factor(c("A", "A", "B", "B", "B"))
  test_exp$expr_mat <- matrix(
    c(
      8,
      8,
      8,
      8,
      8,
      16,
      16,
      32,
      32,
      32,
      4,
      4,
      2,
      2,
      2,
      2,
      2,
      8,
      8,
      8,
      32,
      32,
      16,
      16,
      16
    ),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(
      rownames(test_exp$expr_mat),
      colnames(test_exp$expr_mat)
    )
  )

  result_exp <- transform_alr(test_exp, by = "group", gamma = 0)

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(nrow(result_exp$expr_mat), nrow(test_exp$expr_mat) - 1)
  expect_false("V1" %in% rownames(result_exp$expr_mat))
  expect_false("V1" %in% result_exp$var_info$variable)
  expect_true(all(is.finite(result_exp$expr_mat)))
  expect_equal(unname(result_exp$expr_mat["V2", 1]), 2)
})

test_that("transform_alr falls back to CLR when the reference variance is too high", {
  test_mat <- matrix(
    c(
      2,
      2,
      2,
      16,
      4,
      4,
      4,
      32,
      8,
      8,
      8,
      64
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      paste0("V", 1:3),
      paste0("S", 1:4)
    )
  )
  groups <- factor(c("A", "A", "B", "B"))

  expect_warning(
    result_mat <- transform_alr(test_mat, by = groups, gamma = 0),
    "CLR"
  )

  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(
    result_mat,
    transform_clr(test_mat, by = groups, gamma = 0)
  )
})

test_that("transform_alr allows zeros outside the reference glycan", {
  test_mat <- matrix(
    c(
      8,
      8,
      8,
      8,
      0,
      4,
      0,
      4,
      16,
      32,
      16,
      32
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("V1", "V2", "V3"),
      paste0("S", 1:4)
    )
  )

  result_mat <- transform_alr(test_mat, gamma = 0)

  expect_equal(nrow(result_mat), nrow(test_mat) - 1)
  expect_true(
    identical(sort(rownames(result_mat)), c("V1", "V2")) ||
      identical(sort(rownames(result_mat)), c("V2", "V3"))
  )
  expect_equal(unname(result_mat["V2", "S1"]), 0)
})

test_that("transform_alr uses gamma on the successful ALR path", {
  test_mat <- matrix(
    c(
      8,
      8,
      8,
      8,
      8,
      16,
      16,
      32,
      32,
      32,
      4,
      4,
      2,
      2,
      2,
      2,
      2,
      8,
      8,
      8,
      32,
      32,
      16,
      16,
      16
    ),
    nrow = 5,
    byrow = TRUE,
    dimnames = list(
      paste0("V", 1:5),
      paste0("S", 1:5)
    )
  )
  groups <- factor(c("A", "A", "B", "B", "B"))

  result_det <- transform_alr(test_mat, by = groups, gamma = 0)
  result_stoch <- withr::with_seed(
    1,
    transform_alr(test_mat, by = groups, gamma = 0.1)
  )

  expect_equal(unname(result_stoch["V2", c("S1", "S2")]), c(2, 2))
  expect_false(all(
    unname(result_stoch["V2", c("S3", "S4", "S5")]) == c(4, 4, 4)
  ))
  expect_false(isTRUE(all.equal(result_stoch, result_det)))
})

test_that("transform_alr errors on NA values", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- NA
  expect_error(transform_alr(test_exp, gamma = 0), "NA|missing")
})

test_that("transform_alr errors on negative values", {
  test_exp <- simple_exp(3, 3)
  test_exp$expr_mat[1, 1] <- -1
  expect_error(transform_alr(test_exp, gamma = 0), "negative|positive")
})

test_that("transform_alr errors on unsupported input", {
  expect_error(transform_alr(1), "glyexp_experiment|matrix")
  expect_error(transform_alr("string"), "glyexp_experiment|matrix")
})
