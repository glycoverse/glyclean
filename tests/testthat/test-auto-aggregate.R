test_that("auto_aggregate works for glycoproteomics experiments", {
  exp <- glyexp::real_experiment
  result_exp <- auto_aggregate(exp)
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycoproteomics")
  expect_true(nrow(result_exp) < nrow(exp))
})

test_that("auto_aggregate rejects non-glycoproteomics experiments", {
  exp <- glyexp::real_experiment2
  expect_snapshot(auto_aggregate(exp), error = TRUE)
})

test_that("auto_aggregate works for experiments without glycan structure column", {
  exp <- glyexp::real_experiment
  exp$var_info$glycan_structure <- NULL
  result_exp <- auto_aggregate(exp)
  expect_setequal(
    colnames(result_exp$var_info),
    c("variable", "protein", "gene", "glycan_composition", "protein_site")
  )
})