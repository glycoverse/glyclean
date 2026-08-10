test_that("auto_aggregate works for glycoproteomics experiments", {
  exp <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
  suppressMessages(result_exp <- auto_aggregate(exp))
  expect_glyco_se(result_exp)
  expect_true(glyexp::is_glycoproteomic_se(result_exp))
  expect_true(nrow(result_exp) < nrow(exp))
})

test_that("auto_aggregate works for glycomics experiments", {
  exp <- aggregation_glycomic_se()

  result_exp <- suppressMessages(
    auto_aggregate(exp, standardize_variable = FALSE)
  )

  expect_s4_class(result_exp, "GlycomicSE")
  expect_equal(nrow(result_exp), 3L)
  expect_true(
    "glycan_structure" %in% colnames(SummarizedExperiment::rowData(result_exp))
  )
})

test_that("auto_aggregate uses compositions for glycomics without structures", {
  exp <- aggregation_glycomic_se(include_structure = FALSE)

  result_exp <- suppressMessages(
    auto_aggregate(exp, standardize_variable = FALSE)
  )

  expect_s4_class(result_exp, "GlycomicSE")
  expect_equal(nrow(result_exp), 2L)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(result_exp)),
    "glycan_composition"
  )
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
