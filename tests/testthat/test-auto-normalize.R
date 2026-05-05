test_that("auto_normalize uses deterministic defaults with QC samples", {
  exp <- simple_exp(10, 10)
  exp$meta_data$exp_type <- "glycomics"
  exp$meta_data$glycan_type <- "N"
  exp$sample_info$group <- c(rep("A", 4), rep("B", 4), rep("QC", 2))

  called <- NULL
  testthat::local_mocked_bindings(
    normalize_total_area = function(x) {
      called <<- "normalize_total_area"
      x$expr_mat <- x$expr_mat * 2
      x
    },
    .package = "glyclean"
  )

  expect_snapshot(
    normed <- auto_normalize(
      exp,
      group_col = "group",
      qc_name = "QC",
      to_try = list(normalize_median = function(x) stop("must not be called"))
    ),
    transform = sanitize_cv_snapshot
  )
  expect_equal(called, "normalize_total_area")
  expect_equal(normed$expr_mat, exp$expr_mat * 2)
})

test_that("auto_normalize handles NULL qc_name", {
  exp <- simple_exp(10, 10)
  exp$sample_info$group <- c(rep("A", 4), rep("B", 4), rep("QC", 2))

  expect_snapshot(
    normed <- auto_normalize(exp, group_col = "group", qc_name = NULL),
    transform = sanitize_cv_snapshot
  )
  expect_s3_class(normed, "glyexp_experiment")
})

test_that("auto_normalize works for glycomics without QC", {
  exp <- simple_exp(10, 6)
  exp$meta_data$exp_type <- "glycomics"
  exp$meta_data$glycan_type <- "N" # Required if not others

  manual <- normalize_total_area(exp)
  suppressMessages(auto <- auto_normalize(exp, group_col = NULL))

  expect_equal(auto$expr_mat, manual$expr_mat)
})

test_that("auto_normalize works for glycoproteomics without QC", {
  exp <- simple_exp(10, 6)
  exp$meta_data$exp_type <- "glycoproteomics"
  exp$meta_data$glycan_type <- "N"

  # Manual application
  manual <- normalize_median(exp)

  # Auto application
  expect_snapshot(auto <- auto_normalize(exp, group_col = NULL))

  expect_equal(auto$expr_mat, manual$expr_mat)
})

test_that("auto_normalize falls back to median for others", {
  exp <- simple_exp(10, 6)
  exp$meta_data$exp_type <- "others"

  # Manual application
  manual <- normalize_median(exp)

  # Auto application
  expect_snapshot(auto <- auto_normalize(exp, group_col = NULL))

  expect_equal(auto$expr_mat, manual$expr_mat)
})
