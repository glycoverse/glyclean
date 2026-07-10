# Test main logic path for glycoproteomics data
test_that("auto_clean works for glycoproteomics data", {
  test_exp <- complex_exp()
  expect_snapshot(
    result_exp <- auto_clean(test_exp, standardize_variable = FALSE)
  )

  expect_glyco_se(result_exp)
  expect_true(glyexp::is_glycoproteomic_se(result_exp))
  expect_false(any(is.na(SummarizedExperiment::assay(result_exp))))
})

test_that("auto_clean works for glycoproteomics data with QC", {
  set.seed(123)
  test_exp <- complex_exp()

  # Add QC samples
  qc_mat <- matrix(rnorm(nrow(test_exp) * 2, mean = 10), ncol = 2)
  colnames(qc_mat) <- c("QC1", "QC2")
  expr_mat <- cbind(SummarizedExperiment::assay(test_exp), qc_mat)

  sample_info <- dplyr::bind_rows(
    tibble::as_tibble(
      SummarizedExperiment::colData(test_exp),
      rownames = "sample"
    ),
    tibble::tibble(sample = c("QC1", "QC2"), group = "QC")
  ) |>
    dplyr::mutate(group = factor(group, levels = c("A", "B", "QC")))

  test_exp <- test_glycoproteomic_se(
    expr_mat,
    sample_info,
    tibble::as_tibble(
      SummarizedExperiment::rowData(test_exp),
      rownames = "variable"
    ),
    glycan_type = "N",
    check_col_types = FALSE
  )

  expect_snapshot(
    result_exp <- auto_clean(test_exp, standardize_variable = FALSE),
    transform = sanitize_cv_snapshot
  )

  expect_glyco_se(result_exp)
  expect_true(glyexp::is_glycoproteomic_se(result_exp))
  expect_false(any(is.na(SummarizedExperiment::assay(result_exp))))
})

# Test main logic path for glycomics data
test_that("auto_clean works for glycomics data", {
  # Create proper glycomics experiment
  sample_info <- tibble::tibble(sample = paste0("S", 1:10))
  var_info <- tibble::tibble(
    variable = paste0("V", 1:8),
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 8)
  )
  expr_mat <- matrix(runif(80), nrow = 8)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable

  test_exp <- test_glycomic_se(
    expr_mat,
    sample_info,
    var_info,
    glycan_type = "N"
  )

  expect_snapshot(result_exp <- auto_clean(test_exp))

  expect_glyco_se(result_exp)
  expect_true(glyexp::is_glycomic_se(result_exp))
  expect_false(any(is.na(SummarizedExperiment::assay(result_exp))))
})

test_that("auto_clean works for glycomics data with QC", {
  set.seed(123)
  # Create proper glycomics experiment
  sample_info <- tibble::tibble(
    sample = c(paste0("S", 1:10), "QC1", "QC2"),
    group = factor(c(rep("A", 5), rep("B", 5), "QC", "QC"))
  )
  var_info <- tibble::tibble(
    variable = paste0("V", 1:8),
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 8)
  )
  expr_mat <- matrix(runif(8 * 12), nrow = 8)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable

  test_exp <- test_glycomic_se(
    expr_mat,
    sample_info,
    var_info,
    glycan_type = "N"
  )

  expect_snapshot(
    result_exp <- auto_clean(test_exp),
    transform = sanitize_cv_snapshot
  )

  expect_glyco_se(result_exp)
  expect_true(glyexp::is_glycomic_se(result_exp))
  expect_false(any(is.na(SummarizedExperiment::assay(result_exp))))
})

test_that("auto_clean forwards batch_col to batch correction", {
  set.seed(123)

  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    plate = rep(c("P1", "P2"), each = 6)
  )
  var_info <- tibble::tibble(
    variable = paste0("V", 1:20),
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 20)
  )
  expr_mat <- matrix(nrow = nrow(var_info), ncol = nrow(sample_info))
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  for (i in seq_len(nrow(expr_mat))) {
    plate_effect <- ifelse(sample_info$plate == "P1", 5, 0)
    expr_mat[i, ] <- rnorm(nrow(sample_info), mean = 10 + plate_effect, sd = 1)
  }

  test_exp <- test_glycomic_se(
    expr_mat,
    sample_info,
    var_info,
    glycan_type = "N"
  )

  result_with_batch <- suppressMessages(auto_clean(
    test_exp,
    group_col = NULL,
    batch_col = "plate",
    batch_prop_threshold = 0,
    check_batch_confounding = FALSE
  ))
  result_without_batch <- suppressMessages(auto_clean(
    test_exp,
    group_col = NULL,
    batch_col = NULL,
    batch_prop_threshold = 0,
    check_batch_confounding = FALSE
  ))

  expect_false(isTRUE(all.equal(
    SummarizedExperiment::assay(result_with_batch),
    SummarizedExperiment::assay(result_without_batch)
  )))
})
