test_that("inspect_experiment reports group availability only", {
  exp <- simple_exp(3, 4)
  exp$sample_info$group <- c("A", "QC", "B", "QC")

  info <- inspect_experiment(exp, group_col = "group")

  expect_true(info$has_group)
  expect_named(info, "has_group")

  info <- inspect_experiment(exp, group_col = NULL)
  expect_false(info$has_group)
  expect_named(info, "has_group")
})
