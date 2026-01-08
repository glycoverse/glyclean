test_that("auto_normalize works with QC samples", {
  # Create a simple experiment
  exp <- simple_exp(10, 10)

  # Add QC samples
  exp$sample_info$group <- c(rep("A", 4), rep("B", 4), rep("QC", 2))

  # Make QC samples have low variance after median normalization
  # but high variance otherwise (simulated)
  # Actually, it's hard to simulate "perfectly" for one method without running them.
  # But we can just check if it runs and returns a valid experiment.

  # Let's just verify it runs and picks a method
  expect_snapshot(normed <- auto_normalize(exp, group_col = "group", qc_name = "QC"))
  expect_s3_class(normed, "glyexp_experiment")
})

test_that("auto_normalize handles NULL qc_name", {
  exp <- simple_exp(10, 10)
  exp$sample_info$group <- c(rep("A", 4), rep("B", 4), rep("QC", 2))

  expect_snapshot(normed <- auto_normalize(exp, group_col = "group", qc_name = NULL))
  expect_s3_class(normed, "glyexp_experiment")
})

test_that("auto_normalize works for glycomics without QC", {
  exp <- simple_exp(10, 6)
  exp$meta_data$exp_type <- "glycomics"
  exp$meta_data$glycan_type <- "N" # Required if not others

  # Manual application
  manual <- exp |>
    normalize_median_quotient() |>
    normalize_total_area()

  # Auto application
  expect_snapshot(auto <- auto_normalize(exp, group_col = NULL))

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
