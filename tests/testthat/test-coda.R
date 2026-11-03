test_that("transform_clr works with GlycomicSE input", {
  test_exp <- simple_exp(5, 5)
  SummarizedExperiment::assay(test_exp) <- matrix(
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
      rownames(SummarizedExperiment::assay(test_exp)),
      colnames(SummarizedExperiment::assay(test_exp))
    )
  )
  original_exp <- test_exp
  result_exp <- transform_clr(test_exp, gamma = 0)

  # Check that the function returns the correct structure
  expect_glyco_se(result_exp)
  expect_equal(
    dim(SummarizedExperiment::assay(result_exp)),
    dim(SummarizedExperiment::assay(original_exp))
  )
  expect_equal(
    rownames(SummarizedExperiment::assay(result_exp)),
    rownames(SummarizedExperiment::assay(original_exp))
  )
  expect_equal(
    colnames(SummarizedExperiment::assay(result_exp)),
    colnames(SummarizedExperiment::assay(original_exp))
  )

  expect_equal(
    unname(SummarizedExperiment::assay(result_exp)[, 1]),
    c(-2, 0, 2, -1, 1)
  )
})

test_that("CLR rejects zeros and explains the preprocessing required", {
  mat <- matrix(c(4, 16, 64, 0, 4, 0), nrow = 3)
  expect_error(.transform_clr_mat(mat, gamma = 0), "Zero values")
  expect_error(.transform_clr_mat(mat, gamma = 0), "zero_to_na")
  expect_error(.transform_clr_mat(mat, gamma = 0), "imput")
})

test_that("transform_clr samples per-feature noise on the log2 scale", {
  test_mat <- matrix(
    c(4, 16),
    nrow = 2,
    dimnames = list(c("V1", "V2"), "S1")
  )

  expected_noise <- withr::with_seed(1, stats::rnorm(2, mean = -3, sd = 0.1))
  result_mat <- withr::with_seed(1, .transform_clr_mat(test_mat, gamma = 0.1))

  expect_equal(unname(result_mat[, 1]), c(2, 4) + expected_noise)
})

test_that("CLR rejects entirely zero samples and matrices", {
  for (mat in list(matrix(c(0, 0, 4, 16), nrow = 2), matrix(0, 2, 3))) {
    expect_error(.transform_clr_mat(mat, gamma = 0), "Zero values")
    expect_error(.transform_alr_mat(mat, gamma = 0), "Zero values")
  }
})

test_that("public CoDA transforms are reproducible with their seed", {
  test_exp <- simple_exp(3, 3)
  first <- transform_clr(test_exp, seed = 17)
  second <- transform_clr(test_exp, seed = 17)

  expect_identical(
    SummarizedExperiment::assay(first),
    SummarizedExperiment::assay(second)
  )
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

  result_mat <- .transform_clr_mat(
    test_mat,
    by = groups,
    group_scales = c(A = 1, B = 0.5),
    gamma = 0
  )

  expect_equal(unname(result_mat[, "S1"]), c(-1, 1))
  expect_equal(unname(result_mat[, "S3"]), c(-1, 1))
})

test_that("transform_clr errors on missing values and negative entries", {
  test_exp <- simple_exp(3, 3)
  SummarizedExperiment::assay(test_exp)[1, 1] <- NA
  expect_error(transform_clr(test_exp, gamma = 0), "missing")

  test_exp <- simple_exp(3, 3)
  SummarizedExperiment::assay(test_exp)[1, 1] <- -1
  expect_error(transform_clr(test_exp, gamma = 0), "negative|positive")
})

test_that("transform_clr errors on unsupported input", {
  expect_error(transform_clr(matrix(1:6, nrow = 2)), "glyexp_experiment")
  expect_error(transform_clr(1), "glyexp_experiment")
  expect_error(transform_clr("string"), "glyexp_experiment")
})

test_that("transform_alr works with GlycomicSE input", {
  test_exp <- simple_exp(5, 5)
  SummarizedExperiment::colData(test_exp)$group <- factor(c(
    "A",
    "A",
    "B",
    "B",
    "B"
  ))
  SummarizedExperiment::assay(test_exp) <- matrix(
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
      rownames(SummarizedExperiment::assay(test_exp)),
      colnames(SummarizedExperiment::assay(test_exp))
    )
  )

  result_exp <- transform_alr(test_exp, by = "group", gamma = 0)

  expect_glyco_se(result_exp)
  expect_equal(
    nrow(SummarizedExperiment::assay(result_exp)),
    nrow(SummarizedExperiment::assay(test_exp)) - 1
  )
  expect_false("V1" %in% rownames(SummarizedExperiment::assay(result_exp)))
  expect_false("V1" %in% rownames(result_exp))
  expect_true(all(is.finite(SummarizedExperiment::assay(result_exp))))
  expect_equal(unname(SummarizedExperiment::assay(result_exp)["V2", 1]), 1)
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
    result_mat <- .transform_alr_mat(test_mat, by = groups, gamma = 0),
    "CLR"
  )

  expect_true(is.matrix(result_mat))
  expect_equal(dim(result_mat), dim(test_mat))
  expect_equal(
    result_mat,
    .transform_clr_mat(test_mat, by = groups, gamma = 0)
  )
})

test_that("ALR rejects zeros even outside the reference glycan", {
  mat <- rbind(c(8, 8, 8, 8), c(0, 4, 0, 4), c(16, 32, 16, 32))
  expect_error(.transform_alr_mat(mat, gamma = 0), "Zero values")
  expect_error(.transform_alr_mat(mat, gamma = 0), "zero_to_na")
  expect_error(.transform_alr_mat(mat, gamma = 0), "imput")
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

  result_det <- .transform_alr_mat(test_mat, by = groups, gamma = 0)
  result_stoch <- withr::with_seed(
    1,
    .transform_alr_mat(test_mat, by = groups, gamma = 0.1)
  )

  expect_equal(unname(result_stoch["V2", c("S1", "S2")]), c(1, 1))
  expect_false(all(
    unname(result_stoch["V2", c("S3", "S4", "S5")]) == c(2, 2, 2)
  ))
  expect_false(isTRUE(all.equal(result_stoch, result_det)))
})

test_that("transform_alr errors on NA values", {
  test_exp <- simple_exp(3, 3)
  SummarizedExperiment::assay(test_exp)[1, 1] <- NA
  expect_error(transform_alr(test_exp, gamma = 0), "NA|missing")
})

test_that("transform_alr errors on negative values", {
  test_exp <- simple_exp(3, 3)
  SummarizedExperiment::assay(test_exp)[1, 1] <- -1
  expect_error(transform_alr(test_exp, gamma = 0), "negative|positive")
})

test_that("transform_alr errors on unsupported input", {
  expect_error(transform_alr(matrix(1:6, nrow = 2)), "glyexp_experiment")
  expect_error(transform_alr(1), "glyexp_experiment")
  expect_error(transform_alr("string"), "glyexp_experiment")
})

test_that("ALR reference subtraction and uncertainty stay on the log2 scale", {
  mat <- rbind(
    V1 = c(8, 8, 8, 8),
    V2 = c(16, 4, 32, 2),
    V3 = c(2, 32, 4, 16)
  )
  colnames(mat) <- paste0("S", 1:4)
  groups <- factor(c("A", "A", "B", "B"))
  expected <- log2(sweep(mat[-1L, , drop = FALSE], 2L, mat[1L, ], "/"))
  expect_equal(.apply_alr_reference(mat, 1L, by_values = groups, gamma = 0), expected)

  noise <- withr::with_seed(123, stats::rnorm(2L, mean = 0, sd = 0.2))
  expected[, 3:4] <- sweep(expected[, 3:4, drop = FALSE], 2L, noise, "+")
  noisy <- withr::with_seed(123, .apply_alr_reference(
    mat, 1L, by_values = groups, gamma = 0.2
  ))
  expect_equal(noisy, expected)
  expect_identical(rownames(noisy), c("V2", "V3"))
  expect_identical(colnames(noisy), colnames(mat))
})
