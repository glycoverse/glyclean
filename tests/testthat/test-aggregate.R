test_that("aggregating to glycopeptides works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gp")
  expect_identical(nrow(res), 3L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "peptide", "protein", "gene", "glycan_composition",
      "peptide_site", "protein_site")
  )
})


test_that("aggregating to glycoforms works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gf")
  expect_identical(nrow(res), 2L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "protein", "gene", "glycan_composition", "protein_site")
  )
})


test_that("aggregating to glycopeptides (with structures) works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gps")
  expect_identical(nrow(res), 4L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "peptide", "protein", "gene", "glycan_composition",
      "glycan_structure", "peptide_site", "protein_site")
  )
})


test_that("aggregating to glycoforms (with structures) works", {
  exp <- complex_exp()
  res <- aggregate(exp, to_level = "gfs")
  expect_identical(nrow(res), 3L)
  expect_identical(
    colnames(res$var_info),
    c("variable", "protein", "gene", "glycan_composition", "glycan_structure",
      "protein_site")
  )
})


test_that("aggregating from glycopeptides to glycoforms works", {
  exp <- complex_exp()
  exp <- aggregate(exp, to_level = "gp")
  res <- aggregate(exp, to_level = "gf")
  expect_identical(nrow(res), 2L)
})


test_that("aggregating from glycoforms to glycopeptides fails", {
  exp <- complex_exp()
  exp <- aggregate(exp, to_level = "gf")
  expect_snapshot(aggregate(exp, to_level = "gp"), error = TRUE)
})


test_that("aggregating from glycoforms with structure to glycoforms without structures works", {
  exp <- complex_exp()
  exp <- aggregate(exp, to_level = "gfs")
  res <- aggregate(exp, to_level = "gf")
  expect_identical(nrow(res), 2L)
})


test_that("aggregating from glycoforms without structures to glycoforms with structures fails", {
  exp <- complex_exp()
  exp <- aggregate(exp, to_level = "gf")
  expect_snapshot(aggregate(exp, to_level = "gfs"), error = TRUE)
})
