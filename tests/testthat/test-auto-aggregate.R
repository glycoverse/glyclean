test_that("auto_aggregate is deprecated in favor of aggregate", {
  withr::local_options(lifecycle_verbosity = "warning")
  exp <- aggregation_glycomic_se()

  expect_snapshot(
    result <- auto_aggregate(exp, standardize_variable = FALSE)
  )
  expect_s4_class(result, "GlycomicSE")
})

test_that("auto_aggregate delegates to aggregate", {
  withr::local_options(lifecycle_verbosity = "quiet")
  experiments <- list(aggregation_glycomic_se(), complex_exp())

  for (exp in experiments) {
    result <- auto_aggregate(exp, standardize_variable = FALSE)
    expected <- aggregate(exp, standardize_variable = FALSE)
    expect_identical(class(result), class(expected))
    expect_equal(
      SummarizedExperiment::assay(result),
      SummarizedExperiment::assay(expected)
    )
    expect_equal(
      SummarizedExperiment::rowData(result),
      SummarizedExperiment::rowData(expected)
    )
  }
})
