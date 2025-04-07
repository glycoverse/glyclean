test_that("aggregating to glycopeptides works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gp")
  expect_identical(nrow(res), 3L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "peptide", "proteins", "genes", "glycan_composition",
      "peptide_site", "protein_sites")
  )
})


test_that("aggregating to glycoforms works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gf")
  expect_identical(nrow(res), 2L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "proteins", "genes", "glycan_composition", "protein_sites")
  )
})


test_that("aggregating to glycopeptides (with structures) works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gps")
  expect_identical(nrow(res), 4L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "peptide", "proteins", "genes", "glycan_composition",
      "glycan_structure", "peptide_site", "protein_sites")
  )
})


test_that("aggregating to glycoforms (with structures) works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gfs")
  expect_identical(nrow(res), 3L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "proteins", "genes", "glycan_composition", "glycan_structure",
      "protein_sites")
  )
})
