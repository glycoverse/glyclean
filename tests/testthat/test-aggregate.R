test_that("aggregating to glycopeptides works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gp")
  expect_setequal(
    colnames(res$var_info),
    c("variable", "peptide", "protein", "gene", "glycan_composition",
      "peptide_site", "protein_site")
  )
})

test_that("aggregating to glycoforms works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gf")
  expect_setequal(
    colnames(res$var_info),
    c("variable", "protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregating to glycopeptides (with structures) works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gps")
  expect_setequal(
    colnames(res$var_info),
    c("variable", "peptide", "protein", "gene", "glycan_composition",
      "glycan_structure", "peptide_site", "protein_site")
  )
})

test_that("aggregating to glycoforms (with structures) works", {
  skip("Cannot be easily tested with currenct settings.")
  # The actual result has "peptide" and "peptide_site" columns.
  # This is because these two columns have "many-to-one" relationship with the aggregation columns.
  # That is, one glycoform (with structures) happens to have only one peptide and one peptide site
  # in our test dataset.
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gfs")
  expect_setequal(
    colnames(res$var_info),
    c("variable", "protein", "gene", "glycan_composition", "glycan_structure",
      "protein_site")
  )
})

test_that("aggregating from glycopeptides to glycoforms works", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gp")
  res <- aggregate(exp, to_level = "gf")
  expect_setequal(
    colnames(res$var_info),
    c("variable", "protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregating from glycoforms to glycopeptides fails", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gf")
  expect_error(aggregate(exp, to_level = "gp"), "Missing columns")
})


test_that("aggregating from glycoforms with structure to glycoforms without structures works", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gfs")
  res <- aggregate(exp, to_level = "gf")
  expect_setequal(
    colnames(res$var_info),
    c("variable", "protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregating from glycoforms without structures to glycoforms with structures fails", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gf")
  expect_error(aggregate(exp, to_level = "gfs"), "Missing columns")
})
