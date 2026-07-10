test_that("auto_impute uses deterministic defaults with QC samples", {
  exp <- simple_glycoproteomic_exp(10, 50)
  SummarizedExperiment::assay(exp)[1, 1] <- NA
  SummarizedExperiment::colData(exp)$group <- c(
    rep("A", 24),
    rep("B", 24),
    rep("QC", 2)
  )

  called <- NULL
  testthat::local_mocked_bindings(
    impute_min_prob = function(x, ...) {
      called <<- "impute_min_prob"
      SummarizedExperiment::assay(x)[is.na(SummarizedExperiment::assay(x))] <- 0
      x
    },
    .package = "glyclean"
  )

  expect_snapshot(
    imputed <- auto_impute(
      exp,
      group_col = "group",
      qc_name = "QC",
      to_try = list(impute_sample_min = function(x) stop("must not be called"))
    )
  )
  expect_equal(called, "impute_min_prob")
  expect_false(any(is.na(SummarizedExperiment::assay(imputed))))
})

test_that("auto_impute handles NULL qc_name", {
  exp <- simple_exp(10, 10)
  S4Vectors::metadata(exp)$exp_type <- "glycomics"
  SummarizedExperiment::assay(exp)[1, 1] <- NA
  SummarizedExperiment::assay(exp)[3, 5] <- NA
  SummarizedExperiment::colData(exp)$group <- c(
    rep("A", 4),
    rep("B", 4),
    rep("QC", 2)
  )

  testthat::local_mocked_bindings(
    impute_min_prob = function(x, ...) {
      SummarizedExperiment::assay(x)[is.na(SummarizedExperiment::assay(x))] <- 0
      x
    },
    .package = "glyclean"
  )

  expect_snapshot(
    imputed <- auto_impute(exp, group_col = "group", qc_name = NULL)
  )
  expect_glyco_se(imputed)
  expect_false(any(is.na(SummarizedExperiment::assay(imputed))))
})

test_that("auto_impute chooses defaults by sample count and experiment type", {
  called <- character()
  mark_method <- function(name) {
    function(x, ...) {
      called <<- c(called, name)
      SummarizedExperiment::assay(x)[is.na(SummarizedExperiment::assay(x))] <- 0
      x
    }
  }

  testthat::local_mocked_bindings(
    impute_min_prob = mark_method("impute_min_prob"),
    impute_bpca = mark_method("impute_bpca"),
    impute_miss_forest = mark_method("impute_miss_forest"),
    .package = "glyclean"
  )

  cases <- tibble::tribble(
    ~exp_type         , ~n_samples , ~expected            ,
    "glycomics"       ,         20 , "impute_min_prob"    ,
    "glycoproteomics" ,         20 , "impute_min_prob"    ,
    "glycomics"       ,         50 , "impute_bpca"        ,
    "glycoproteomics" ,         50 , "impute_min_prob"    ,
    "glycomics"       ,        120 , "impute_miss_forest" ,
    "glycoproteomics" ,        120 , "impute_bpca"
  )

  purrr::pwalk(cases, function(exp_type, n_samples, expected) {
    exp <- if (exp_type == "glycomics") {
      simple_exp(10, n_samples)
    } else {
      simple_glycoproteomic_exp(10, n_samples)
    }
    SummarizedExperiment::assay(exp)[1, 1] <- NA

    suppressMessages(auto <- auto_impute(exp, group_col = NULL))
    expect_false(any(is.na(SummarizedExperiment::assay(auto))))
  })

  expect_equal(called, cases$expected)
})

test_that("auto_impute uses min_prob for others experiments", {
  exp <- legacy_exp(10, 10)
  exp$expr_mat[1, 1] <- NA

  called <- NULL
  testthat::local_mocked_bindings(
    impute_min_prob = function(x, ...) {
      called <<- "impute_min_prob"
      x$expr_mat[is.na(x$expr_mat)] <- 0
      x
    },
    .package = "glyclean"
  )

  expect_message(
    result <- auto_impute(exp, group_col = NULL),
    'Reason: default for "others" with n_samples = 10\\.'
  )
  expect_equal(called, "impute_min_prob")
  expect_s3_class(result, "glyexp_experiment")
  expect_false(any(is.na(result$expr_mat)))
})

test_that("auto_impute rejects unsupported experiment types", {
  exp <- legacy_exp(10, 10, exp_type = "traitomics", glycan_type = "N")
  expect_snapshot(error = TRUE, auto_impute(exp, group_col = NULL))

  exp <- legacy_exp(10, 10, exp_type = "traitproteomics", glycan_type = "N")
  expect_snapshot(error = TRUE, auto_impute(exp, group_col = NULL))
})

test_that("auto_impute handles missing group_col gracefully", {
  exp <- simple_glycoproteomic_exp(10, 10)
  SummarizedExperiment::assay(exp)[1, 1] <- NA

  testthat::local_mocked_bindings(
    impute_min_prob = function(x, ...) {
      SummarizedExperiment::assay(x)[is.na(SummarizedExperiment::assay(x))] <- 0
      x
    },
    .package = "glyclean"
  )

  # When group_col is NULL, should use default strategy
  expect_snapshot(result <- auto_impute(exp, group_col = NULL))
  expect_glyco_se(result)
  expect_false(any(is.na(SummarizedExperiment::assay(result))))
})

test_that("auto_impute handles non-existent group_col gracefully", {
  exp <- simple_glycoproteomic_exp(10, 10)
  SummarizedExperiment::assay(exp)[1, 1] <- NA

  testthat::local_mocked_bindings(
    impute_min_prob = function(x, ...) {
      SummarizedExperiment::assay(x)[is.na(SummarizedExperiment::assay(x))] <- 0
      x
    },
    .package = "glyclean"
  )

  # When group_col doesn't exist, should use default strategy
  expect_snapshot(result <- auto_impute(exp, group_col = "nonexistent"))
  expect_glyco_se(result)
  expect_false(any(is.na(SummarizedExperiment::assay(result))))
})

test_that("auto_impute validates input and ignores deprecated arguments", {
  exp <- simple_glycoproteomic_exp(10, 10)
  SummarizedExperiment::assay(exp)[1, 1] <- NA

  testthat::local_mocked_bindings(
    impute_min_prob = function(x, ...) {
      SummarizedExperiment::assay(x)[is.na(SummarizedExperiment::assay(x))] <- 0
      x
    },
    .package = "glyclean"
  )

  # Test invalid exp
  expect_error(auto_impute("not_an_experiment"), "glyexp_experiment")

  expect_snapshot(
    result <- auto_impute(
      exp,
      qc_name = NULL,
      to_try = "not_a_list"
    )
  )
  expect_glyco_se(result)
  expect_false(any(is.na(SummarizedExperiment::assay(result))))
})
