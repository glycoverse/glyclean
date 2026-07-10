test_that("auto_aggregate works for glycoproteomics experiments", {
  exp <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
  suppressMessages(result_exp <- auto_aggregate(exp))
  expect_glyco_se(result_exp)
  expect_true(glyexp::is_glycoproteomic_se(result_exp))
  expect_true(nrow(result_exp) < nrow(exp))
})

test_that("auto_aggregate rejects non-glycoproteomics experiments", {
  exp <- glyexp::as_glycomic_se(glyexp::real_experiment2)
  expect_snapshot(auto_aggregate(exp), error = TRUE)
})

test_that("auto_aggregate works for experiments without glycan structure column", {
  exp <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
  SummarizedExperiment::rowData(exp)$glycan_structure <- NULL
  suppressMessages(result_exp <- auto_aggregate(exp))
  expect_setequal(
    colnames(SummarizedExperiment::rowData(result_exp)),
    c("protein", "gene", "glycan_composition", "protein_site")
  )
})
