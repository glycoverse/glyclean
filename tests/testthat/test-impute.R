missing_exp_3_3 <- function() {
  exp <- simple_exp(3, 3)
  old_expr_mat <- SummarizedExperiment::assay(exp)
  new_expr_mat <- matrix(
    c(1, 1, NA, NA, 2, 4, 1, 3, 6),
    nrow = 3,
    byrow = TRUE
  )
  colnames(new_expr_mat) <- colnames(old_expr_mat)
  rownames(new_expr_mat) <- rownames(old_expr_mat)
  SummarizedExperiment::assay(exp) <- new_expr_mat
  exp
}


missing_exp_10_10 <- function() {
  exp <- simple_exp(10, 10)
  SummarizedExperiment::assay(exp)[c(1, 5, 29, 68, 45, 100)] <- NA
  exp
}


test_that("data can be loaded", {
  expect_no_error(test_exp <<- simple_exp(10, 10))
  # Set seed for reproducible random sampling
  set.seed(999)
  # Add some NA values for testing
  SummarizedExperiment::assay(test_exp)[sample(
    length(SummarizedExperiment::assay(test_exp)),
    20
  )] <- NA
  expect_no_error(old_test_exp <<- test_exp)
})


test_that("impute_zero works", {
  result_exp <- impute_zero(test_exp)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
  # Check that NA values were replaced with 0
  na_positions <- which(is.na(SummarizedExperiment::assay(test_exp)))
  expect_true(all(SummarizedExperiment::assay(result_exp)[na_positions] == 0))
})


test_that("impute_sample_min works", {
  result_exp <- impute_sample_min(old_test_exp)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)

  # Test that the filled values are actually the minimum values in each column
  old_mat <- SummarizedExperiment::assay(old_test_exp)
  new_mat <- SummarizedExperiment::assay(result_exp)

  for (i in seq_len(ncol(old_mat))) {
    if (any(is.na(old_mat[, i]))) {
      col_min <- min(old_mat[, i], na.rm = TRUE)
      old_na_indices <- which(is.na(old_mat[, i]))
      filled_values <- new_mat[old_na_indices, i]
      expect_true(all(filled_values == col_min))
    }
  }
})


test_that("impute_half_sample_min works", {
  result_exp <- impute_half_sample_min(old_test_exp)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)

  # Test that the filled values are actually half the minimum values in each column
  old_mat <- SummarizedExperiment::assay(old_test_exp)
  new_mat <- SummarizedExperiment::assay(result_exp)

  for (i in seq_len(ncol(old_mat))) {
    if (any(is.na(old_mat[, i]))) {
      col_min <- min(old_mat[, i], na.rm = TRUE)
      old_na_indices <- which(is.na(old_mat[, i]))
      filled_values <- new_mat[old_na_indices, i]
      expect_true(all(filled_values == col_min / 2))
    }
  }
})


test_that("impute_sw_knn works", {
  skip_if_not_installed("impute")
  result_exp <- impute_sw_knn(test_exp, k = 5)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
})


test_that("impute_fw_knn works", {
  skip_if_not_installed("impute")
  result_exp <- impute_fw_knn(test_exp, k = 5)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
})


test_that("impute_bpca works", {
  skip_if_not_installed("pcaMethods")
  result_exp <- impute_bpca(test_exp)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
})


# test_that("impute_ppca works", {
#   skip_if_not_installed("pcaMethods")
#   result_exp <- impute_ppca(test_exp)
#   expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
# })

test_that("impute_svd works", {
  skip_if_not_installed("pcaMethods")
  result_exp <- impute_svd(test_exp)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
})


test_that("impute_min_prob works", {
  result_exp <- impute_min_prob(test_exp)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
})

test_that("impute_min_prob uses left-censored log-scale draws", {
  test_mat <- matrix(
    c(
      2,
      4,
      8,
      4,
      NA,
      16,
      NA,
      8,
      32,
      16,
      32,
      64
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(test_mat) <- paste0("V", 1:4)
  colnames(test_mat) <- paste0("S", 1:3)

  normed <- log2(test_mat)
  sample_mins <- purrr::map_dbl(
    seq_len(ncol(normed)),
    ~ unname(stats::quantile(normed[, .x], probs = 0.1, na.rm = TRUE))
  )
  feature_sds <- purrr::map_dbl(
    which(rowMeans(!is.na(normed)) > 0.5),
    ~ stats::sd(normed[.x, ], na.rm = TRUE)
  )
  draw_sd <- stats::median(feature_sds, na.rm = TRUE) * 0.25

  expected <- normed
  set.seed(42)
  purrr::walk(seq_len(ncol(normed)), function(sample_idx) {
    sample_draws <- stats::rnorm(
      nrow(normed),
      mean = sample_mins[sample_idx],
      sd = draw_sd
    )
    missing_idx <- which(is.na(normed[, sample_idx]))
    expected[missing_idx, sample_idx] <<- sample_draws[missing_idx]
  })

  set.seed(42)
  result <- .impute_min_prob(test_mat, q = 0.1, tune.sigma = 0.25)

  expect_equal(result, 2^expected)
  expect_equal(result[!is.na(test_mat)], test_mat[!is.na(test_mat)])
  expect_equal(dimnames(result), dimnames(test_mat))
})

test_that("impute_min_prob falls back to global center for all-missing samples", {
  test_mat <- matrix(
    c(
      2,
      NA,
      8,
      4,
      NA,
      16,
      8,
      NA,
      32,
      16,
      NA,
      64
    ),
    nrow = 4,
    byrow = TRUE
  )
  rownames(test_mat) <- paste0("V", 1:4)
  colnames(test_mat) <- paste0("S", 1:3)

  set.seed(42)
  result <- .impute_min_prob(test_mat, q = 0.1, tune.sigma = 0)

  expect_equal(sum(is.na(result)), 0)
  expect_equal(result[, c("S1", "S3")], test_mat[, c("S1", "S3")])
  expect_true(all(is.finite(result[, "S2"])))
})

test_that("impute_min_prob validates unsupported dots clearly", {
  test_exp <- simple_exp(2, 2)
  SummarizedExperiment::assay(test_exp)[2] <- NA

  expect_error(
    impute_min_prob(test_exp, unsupported = TRUE),
    "Unsupported argument"
  )
})


test_that("impute_miss_forest works", {
  skip_if_not_installed("missForest")
  result_exp <- impute_miss_forest(test_exp)
  expect_equal(sum(is.na(SummarizedExperiment::assay(result_exp))), 0)
})


test_that("imputation functions reject inputs outside supported containers", {
  funcs <- list(
    impute_zero,
    impute_sample_min,
    impute_half_sample_min,
    impute_sw_knn,
    impute_fw_knn,
    impute_bpca,
    impute_ppca,
    impute_svd,
    impute_min_prob,
    impute_miss_forest
  )

  purrr::walk(funcs, function(fn) {
    expect_error(fn(matrix(1:6, nrow = 2)), "glyexp_experiment")
    expect_error(fn(1), "glyexp_experiment")
  })
})

test_that("impute_min_prob is self-contained", {
  deps <- packageDescription("glyclean")[c("Imports", "Suggests")]
  deps <- paste(unlist(deps), collapse = "\n")

  expect_false(grepl("imputeLCMD", deps, fixed = TRUE))
})

test_that("impute methods requiring suggested packages run or error cleanly", {
  test_exp <- simple_exp(6, 6)
  SummarizedExperiment::assay(test_exp)[] <- runif(36, 1, 100)
  SummarizedExperiment::assay(test_exp)[1, 2] <- NA

  cases <- list(
    list(fn = impute_sw_knn, pkg = "impute", args = list(k = 2)),
    list(fn = impute_fw_knn, pkg = "impute", args = list(k = 2)),
    list(fn = impute_bpca, pkg = "pcaMethods", args = list()),
    list(fn = impute_ppca, pkg = "pcaMethods", args = list()),
    list(fn = impute_svd, pkg = "pcaMethods", args = list()),
    list(fn = impute_miss_forest, pkg = "missForest", args = list(seed = 1))
  )

  purrr::walk(cases, function(case) {
    if (rlang::is_installed(case$pkg)) {
      result <- suppressWarnings(do.call(case$fn, c(list(test_exp), case$args)))
      expect_glyco_se(result)
      expect_equal(sum(is.na(SummarizedExperiment::assay(result))), 0)
    } else {
      expect_error(do.call(case$fn, c(list(test_exp), case$args)), case$pkg)
    }
  })
})
