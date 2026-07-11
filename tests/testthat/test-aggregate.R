test_that("aggregating to glycopeptides works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gp", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c(
      "peptide",
      "protein",
      "gene",
      "glycan_composition",
      "peptide_site",
      "protein_site"
    )
  )
})

test_that("aggregating to glycoforms works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c("protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregation preserves group order, missing-value sums, and metadata", {
  exp <- complex_exp()
  input_mat <- SummarizedExperiment::assay(exp)
  first_group <- c(1, 2, 4, 5, 6, 7)
  input_mat[first_group, 1] <- NA_real_
  input_mat[1, 2] <- NA_real_
  SummarizedExperiment::assay(exp) <- input_mat

  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  result_mat <- SummarizedExperiment::assay(res)
  result_var_info <- SummarizedExperiment::rowData(res)
  expected_mat <- rbind(
    colSums(input_mat[first_group, , drop = FALSE], na.rm = TRUE),
    input_mat[3, ],
    input_mat[8, ]
  )
  rownames(expected_mat) <- paste0("V", seq_len(nrow(expected_mat)))

  expect_equal(result_mat, expected_mat)
  expect_equal(result_var_info$protein_site, c(24L, 25L, 24L))
  expected_compositions <- SummarizedExperiment::rowData(
    exp
  )$glycan_composition[
    c(1, 3, 8)
  ]
  expect_equal(
    as.character(result_var_info$glycan_composition),
    as.character(expected_compositions)
  )
  expect_true(
    glyrepr::is_glycan_composition(result_var_info$glycan_composition)
  )
  expect_true("gene" %in% colnames(result_var_info))
  expect_false("peptide" %in% colnames(result_var_info))
  expect_false("charge" %in% colnames(result_var_info))
})

test_that("aggregating to glycopeptides (with structures) works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gps", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c(
      "peptide",
      "protein",
      "gene",
      "glycan_composition",
      "glycan_structure",
      "peptide_site",
      "protein_site"
    )
  )
})

test_that("aggregating to glycoforms (with structures) works", {
  skip("Cannot be easily tested with currenct settings.")
  # The actual result has "peptide" and "peptide_site" columns.
  # This is because these two columns have "many-to-one" relationship with the aggregation columns.
  # That is, one glycoform (with structures) happens to have only one peptide and one peptide site
  # in our test dataset.
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gfs", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c(
      "protein",
      "gene",
      "glycan_composition",
      "glycan_structure",
      "protein_site"
    )
  )
})

test_that("aggregating from glycopeptides to glycoforms works", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gp", standardize_variable = FALSE)
  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c("protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregating from glycoforms to glycopeptides fails", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_snapshot(
    aggregate(exp, to_level = "gp", standardize_variable = FALSE),
    error = TRUE
  )
})


test_that("aggregating from glycoforms with structure to glycoforms without structures works", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gfs", standardize_variable = FALSE)
  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c("protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregating from glycoforms without structures to glycoforms with structures fails", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_snapshot(
    aggregate(exp, to_level = "gfs", standardize_variable = FALSE),
    error = TRUE
  )
})
