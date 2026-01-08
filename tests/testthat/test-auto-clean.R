replace_cv <- function(x) {
  x <- stringr::str_replace_all(x, "CV = \\d+\\.\\d+", "CV = CV_VALUE")
  x <- stringr::str_replace_all(x, 'Best method: ".*?"', 'Best method: "BEST_METHOD"')
  x
}

# Test main logic path for glycoproteomics data
test_that("auto_clean works for glycoproteomics data", {
  test_exp <- complex_exp()
  expect_snapshot(result_exp <- auto_clean(test_exp))

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycoproteomics")
  expect_false(any(is.na(result_exp$expr_mat)))
})

test_that("auto_clean works for glycoproteomics data with QC", {
  set.seed(123)
  test_exp <- complex_exp()

  # Add QC samples
  qc_mat <- matrix(rnorm(nrow(test_exp) * 2, mean = 10), ncol = 2)
  colnames(qc_mat) <- c("QC1", "QC2")
  expr_mat <- cbind(test_exp$expr_mat, qc_mat)

  sample_info <- dplyr::bind_rows(
    test_exp$sample_info,
    tibble::tibble(sample = c("QC1", "QC2"), group = "QC")
  )

  test_exp <- glyexp::experiment(
    expr_mat,
    sample_info,
    test_exp$var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    check_col_types = FALSE
  )

  expect_snapshot(result_exp <- auto_clean(test_exp), transform = replace_cv)

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycoproteomics")
  expect_false(any(is.na(result_exp$expr_mat)))
})

test_that("auto_clean works with NULL qc_name", {
  set.seed(123)
  test_exp <- complex_exp()

  # Add QC samples
  qc_mat <- matrix(rnorm(nrow(test_exp) * 2, mean = 10), ncol = 2)
  colnames(qc_mat) <- c("QC1", "QC2")
  expr_mat <- cbind(test_exp$expr_mat, qc_mat)

  sample_info <- dplyr::bind_rows(
    test_exp$sample_info,
    tibble::tibble(sample = c("QC1", "QC2"), group = "QC")
  )

  test_exp <- glyexp::experiment(
    expr_mat,
    sample_info,
    test_exp$var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    check_col_types = FALSE
  )

  expect_snapshot(result_exp <- auto_clean(test_exp, qc_name = NULL))

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycoproteomics")
  expect_false(any(is.na(result_exp$expr_mat)))
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

  test_exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycomics",
    glycan_type = "N"
  )

  expect_snapshot(result_exp <- auto_clean(test_exp))

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycomics")
  expect_false(any(is.na(result_exp$expr_mat)))
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

  test_exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycomics",
    glycan_type = "N"
  )

  expect_snapshot(result_exp <- auto_clean(test_exp), transform = replace_cv)

  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycomics")
  expect_false(any(is.na(result_exp$expr_mat)))
})
