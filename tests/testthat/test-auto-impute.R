test_that("auto_impute uses deterministic defaults with QC samples", {
  skip_if_not_installed("mockr")

  exp <- simple_exp(10, 50)
  exp$meta_data$exp_type <- "glycoproteomics"
  exp$expr_mat[1, 1] <- NA
  exp$sample_info$group <- c(rep("A", 24), rep("B", 24), rep("QC", 2))

  called <- NULL
  mockr::local_mock(
    impute_min_prob = function(x, ...) {
      called <<- "impute_min_prob"
      x$expr_mat[is.na(x$expr_mat)] <- 0
      x
    }
  )

  expect_snapshot(
    imputed <- auto_impute(
      exp,
      group_col = "group",
      qc_name = "QC",
      to_try = list(impute_sample_min = function(x) stop("must not be called"))
    )
  )
  expect_equal(called, "impute_min_prob")
  expect_false(any(is.na(imputed$expr_mat)))
})

test_that("auto_impute handles NULL qc_name", {
  skip_if_not_installed("mockr")

  exp <- simple_exp(10, 10)
  exp$meta_data$exp_type <- "glycomics"
  exp$expr_mat[1, 1] <- NA
  exp$expr_mat[3, 5] <- NA
  exp$sample_info$group <- c(rep("A", 4), rep("B", 4), rep("QC", 2))

  mockr::local_mock(
    impute_min_prob = function(x, ...) {
      x$expr_mat[is.na(x$expr_mat)] <- 0
      x
    }
  )

  expect_snapshot(
    imputed <- auto_impute(exp, group_col = "group", qc_name = NULL)
  )
  expect_s3_class(imputed, "glyexp_experiment")
  expect_false(any(is.na(imputed$expr_mat)))
})

test_that("auto_impute chooses defaults by sample count and experiment type", {
  skip_if_not_installed("mockr")

  called <- character()
  mark_method <- function(name) {
    function(x, ...) {
      called <<- c(called, name)
      x$expr_mat[is.na(x$expr_mat)] <- 0
      x
    }
  }

  mockr::local_mock(
    impute_min_prob = mark_method("impute_min_prob"),
    impute_bpca = mark_method("impute_bpca"),
    impute_miss_forest = mark_method("impute_miss_forest")
  )

  cases <- tibble::tribble(
    ~exp_type, ~n_samples, ~expected,
    "glycomics", 20, "impute_min_prob",
    "glycoproteomics", 20, "impute_min_prob",
    "glycomics", 50, "impute_bpca",
    "glycoproteomics", 50, "impute_min_prob",
    "glycomics", 120, "impute_miss_forest",
    "glycoproteomics", 120, "impute_bpca"
  )

  purrr::pwalk(cases, function(exp_type, n_samples, expected) {
    exp <- simple_exp(10, n_samples)
    exp$meta_data$exp_type <- exp_type
    exp$expr_mat[1, 1] <- NA

    suppressMessages(auto <- auto_impute(exp, group_col = NULL))
    expect_false(any(is.na(auto$expr_mat)))
  })

  expect_equal(called, cases$expected)
})

test_that("auto_impute handles missing group_col gracefully", {
  skip_if_not_installed("mockr")

  exp <- simple_exp(10, 10)
  exp$meta_data$exp_type <- "glycoproteomics"
  exp$expr_mat[1, 1] <- NA

  mockr::local_mock(
    impute_min_prob = function(x, ...) {
      x$expr_mat[is.na(x$expr_mat)] <- 0
      x
    }
  )

  # When group_col is NULL, should use default strategy
  expect_snapshot(result <- auto_impute(exp, group_col = NULL))
  expect_s3_class(result, "glyexp_experiment")
  expect_false(any(is.na(result$expr_mat)))
})

test_that("auto_impute handles non-existent group_col gracefully", {
  skip_if_not_installed("mockr")

  exp <- simple_exp(10, 10)
  exp$meta_data$exp_type <- "glycoproteomics"
  exp$expr_mat[1, 1] <- NA

  mockr::local_mock(
    impute_min_prob = function(x, ...) {
      x$expr_mat[is.na(x$expr_mat)] <- 0
      x
    }
  )

  # When group_col doesn't exist, should use default strategy
  expect_snapshot(result <- auto_impute(exp, group_col = "nonexistent"))
  expect_s3_class(result, "glyexp_experiment")
  expect_false(any(is.na(result$expr_mat)))
})

test_that("auto_impute validates input arguments", {
  exp <- simple_exp(10, 10)

  # Test invalid exp
  expect_error(auto_impute("not_an_experiment"), "glyexp_experiment")

  # Test invalid to_try
  expect_error(auto_impute(exp, to_try = "not_a_list"), "list")
  expect_error(auto_impute(exp, to_try = list("not_a_function")), "function")
})
