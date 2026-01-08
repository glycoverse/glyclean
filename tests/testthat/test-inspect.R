test_that("inspect_experiment handles NULL qc_name", {
  exp <- simple_exp(3, 4)
  exp$sample_info$group <- c("A", "QC", "B", "QC")

  info <- inspect_experiment(exp, group_col = "group", qc_name = NULL)

  expect_true(info$has_group)
  expect_false(info$has_qc)
  expect_null(info$qc_samples)
})
