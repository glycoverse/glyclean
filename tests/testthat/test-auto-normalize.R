test_that("auto_normalize uses deterministic defaults with QC samples", {
  exp <- simple_exp(10, 10)
  SummarizedExperiment::colData(exp)$group <- c(
    rep("A", 4),
    rep("B", 4),
    rep("QC", 2)
  )

  called <- NULL
  testthat::local_mocked_bindings(
    normalize_total_area = function(x) {
      called <<- "normalize_total_area"
      SummarizedExperiment::assay(x) <- SummarizedExperiment::assay(x) * 2
      x
    },
    .package = "glyclean"
  )

  expect_snapshot(
    normed <- auto_normalize(
      exp,
      group_col = "group"
    ),
    transform = sanitize_cv_snapshot
  )
  expect_equal(called, "normalize_total_area")
  expect_equal(
    SummarizedExperiment::assay(normed),
    SummarizedExperiment::assay(exp) * 2
  )
})

test_that("auto_normalize works for glycomics without QC", {
  exp <- simple_exp(10, 6)

  manual <- normalize_total_area(exp)
  suppressMessages(auto <- auto_normalize(exp, group_col = NULL))

  expect_equal(
    SummarizedExperiment::assay(auto),
    SummarizedExperiment::assay(manual)
  )
})

test_that("auto_normalize works for glycoproteomics without QC", {
  exp <- simple_glycoproteomic_exp(10, 6)

  # Manual application
  manual <- normalize_median(exp)

  # Auto application
  expect_snapshot(auto <- auto_normalize(exp, group_col = NULL))

  expect_equal(
    SummarizedExperiment::assay(auto),
    SummarizedExperiment::assay(manual)
  )
})

test_that("auto_normalize falls back to median for others", {
  exp <- legacy_exp(10, 6)

  # Manual application
  manual <- normalize_median(exp)

  # Auto application
  expect_snapshot(auto <- auto_normalize(exp, group_col = NULL))

  expect_equal(auto$expr_mat, manual$expr_mat)
})
