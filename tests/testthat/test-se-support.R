test_that("assay preprocessing preserves glyco SE subclasses", {
  for (x in list(simple_glycomic_se(), simple_glycoproteomic_se())) {
    class_name <- class(x)[[1]]
    SummarizedExperiment::assay(x)[1, 1] <- NA_real_

    normalized <- normalize_median(x)
    imputed <- impute_zero(x)

    expect_s4_class(normalized, class_name)
    expect_s4_class(imputed, class_name)
    expect_false(anyNA(SummarizedExperiment::assay(imputed)))
    expect_identical(
      S4Vectors::metadata(normalized),
      S4Vectors::metadata(x)
    )
  }
})

test_that("non-auto preprocessing accepts plain SummarizedExperiment objects", {
  x <- plain_se(simple_glycomic_se())
  SummarizedExperiment::assay(x)[1, 1] <- NA_real_
  SummarizedExperiment::colData(x)$batch <- factor(c(
    "A",
    "A",
    "A",
    "B",
    "B",
    "B"
  ))

  normalized <- normalize_median(x)
  imputed <- impute_zero(x)
  filtered <- suppressMessages(remove_rare(x))
  transformed <- transform_clr(imputed, gamma = 0)
  corrected <- suppressMessages(
    correct_batch_effect(x, batch = "batch", group = "group")
  )

  for (result in list(normalized, imputed, filtered, transformed, corrected)) {
    expect_identical(as.character(class(result)), "SummarizedExperiment")
  }
  expect_false(anyNA(SummarizedExperiment::assay(imputed)))
  expect_identical(S4Vectors::metadata(normalized), S4Vectors::metadata(x))
})

test_that("assay preprocessing preserves a nonstandard assay name", {
  x <- simple_glycomic_se()
  SummarizedExperiment::assayNames(x) <- "signal"

  result <- normalize_median(x)

  expect_s4_class(result, "GlycomicSE")
  expect_identical(SummarizedExperiment::assayNames(result), "signal")
})

test_that("grouped preprocessing reads grouping variables from colData", {
  x <- simple_glycomic_se()
  SummarizedExperiment::assay(x)[1, 1:4] <- NA_real_

  result <- suppressMessages(remove_rare(x, by = "group", prop = 0.5))

  expect_s4_class(result, "GlycomicSE")
  expect_identical(
    rownames(result),
    rownames(x)[-1]
  )
})

test_that("automatic preprocessing preserves glyco SE subclasses", {
  glycomic <- simple_glycomic_se()
  glycoproteomic <- simple_glycoproteomic_se()

  expect_s4_class(
    suppressMessages(auto_normalize(glycomic)),
    "GlycomicSE"
  )
  expect_s4_class(
    suppressMessages(auto_normalize(glycoproteomic)),
    "GlycoproteomicSE"
  )
  expect_s4_class(
    suppressMessages(auto_impute(glycomic)),
    "GlycomicSE"
  )
  expect_s4_class(
    suppressMessages(auto_remove(glycomic, preset = "simple")),
    "GlycomicSE"
  )
  expect_s4_class(
    suppressMessages(auto_coda(glycomic, gamma = 0)),
    "GlycomicSE"
  )
  expect_s4_class(
    suppressMessages(
      auto_aggregate(glycoproteomic, standardize_variable = FALSE)
    ),
    "GlycoproteomicSE"
  )
  expect_s4_class(
    suppressMessages(auto_clean(glycomic, standardize_variable = FALSE)),
    "GlycomicSE"
  )
  expect_s4_class(
    suppressMessages(
      auto_clean(glycoproteomic, standardize_variable = FALSE)
    ),
    "GlycoproteomicSE"
  )
})

test_that("automatic preprocessing still requires a typed container", {
  x <- plain_se(simple_glycomic_se())

  expect_error(
    auto_normalize(x),
    "Must inherit from class 'glyexp_experiment', 'GlycomicSE', or 'GlycoproteomicSE'",
    fixed = TRUE
  )
})

test_that("CoDA transformations preserve class and store SE metadata", {
  x <- simple_glycomic_se()

  clr <- transform_clr(x, gamma = 0)
  alr <- suppressWarnings(transform_alr(x, gamma = 0))

  expect_s4_class(clr, "GlycomicSE")
  expect_identical(S4Vectors::metadata(clr)$coda_transform, "clr")
  expect_s4_class(alr, "GlycomicSE")
  expect_true(S4Vectors::metadata(alr)$coda_transform %in% c("alr", "clr"))
  expect_true(nrow(alr) %in% c(nrow(x) - 1, nrow(x)))
})

test_that("batch functions accept and preserve glyco SE subclasses", {
  x <- simple_glycomic_se(n_var = 10, n_samp = 12)
  SummarizedExperiment::colData(x)$batch <- factor(rep(c("A", "B"), each = 6))

  detected <- detect_batch_effect(x, batch = "batch", group = "group")
  corrected <- suppressMessages(
    correct_batch_effect(x, batch = "batch", group = "group")
  )

  expect_length(detected, nrow(x))
  expect_s4_class(corrected, "GlycomicSE")
})

test_that("glycoproteomics operations preserve GlycoproteomicSE", {
  x <- simple_glycoproteomic_se()
  proteins <- unique(as.character(SummarizedExperiment::rowData(x)$protein))
  pro_expr_mat <- matrix(
    1,
    nrow = length(proteins),
    ncol = ncol(x),
    dimnames = list(proteins, colnames(x))
  )
  fasta <- stats::setNames(
    rep(paste(rep("A", 1000), collapse = ""), length(proteins)),
    proteins
  )

  aggregated <- suppressMessages(
    aggregate(x, to_level = "gf", standardize_variable = FALSE)
  )
  adjusted <- suppressMessages(adjust_protein(x, pro_expr_mat))
  annotated <- suppressMessages(add_site_seq(x, fasta = fasta, n_aa = 2))

  expect_s4_class(aggregated, "GlycoproteomicSE")
  expect_s4_class(adjusted, "GlycoproteomicSE")
  expect_s4_class(annotated, "GlycoproteomicSE")
  expect_true(
    "site_sequence" %in% colnames(SummarizedExperiment::rowData(annotated))
  )
})

test_that("specialized glycoproteomics operations reject plain SummarizedExperiment", {
  x <- plain_se(simple_glycoproteomic_se())
  message <- "must be a <glyexp_experiment> object or a <GlycoproteomicSE> object"

  expect_error(
    aggregate(x, standardize_variable = FALSE),
    message,
    fixed = TRUE
  )
  expect_error(
    adjust_protein(x, matrix(1)),
    message,
    fixed = TRUE
  )
  expect_error(
    add_site_seq(x),
    message,
    fixed = TRUE
  )
})

test_that("specialized glycoproteomics operations distinguish glyco SE subclasses", {
  x <- simple_glycomic_se()
  message <- "Got <GlycomicSE>."

  expect_error(
    aggregate(x, standardize_variable = FALSE),
    message,
    fixed = TRUE
  )
  expect_error(
    adjust_protein(x, matrix(1)),
    message,
    fixed = TRUE
  )
  expect_error(
    add_site_seq(x),
    message,
    fixed = TRUE
  )
})

test_that("specialized glycoproteomics operations retain experiment type errors", {
  x <- glyexp::real_experiment2
  message <- 'Got "glycomics".'

  expect_error(
    aggregate(x, standardize_variable = FALSE),
    message,
    fixed = TRUE
  )
  expect_error(
    adjust_protein(x, matrix(1)),
    message,
    fixed = TRUE
  )
  expect_error(
    add_site_seq(x),
    message,
    fixed = TRUE
  )
})

test_that("standardization bridges subclasses without exp_type metadata", {
  testthat::local_mocked_bindings(
    standardize_variable = function(exp) exp,
    .package = "glyexp"
  )

  inputs <- list(
    GlycomicSE = simple_glycomic_se(),
    GlycoproteomicSE = complex_exp()
  )

  for (class_name in names(inputs)) {
    x <- inputs[[class_name]]
    expect_null(S4Vectors::metadata(x)$exp_type)

    result <- glyclean:::.standardize_container_variable(x)

    expect_s4_class(result, class_name)
    expect_identical(S4Vectors::metadata(result)$glycan_type, "N")
  }
})

test_that("QC functions accept glyco SE subclasses", {
  x <- simple_glycomic_se()

  expect_s3_class(plot_missing_bar(x), "ggplot")
  expect_s3_class(plot_tic_bar(x), "ggplot")
  expect_s3_class(plot_rank_abundance(x), "ggplot")
  expect_s3_class(plot_int_boxplot(x, by = "group"), "ggplot")
  expect_s3_class(plot_rle(x, by = "group"), "ggplot")
  expect_s3_class(plot_cv_dent(x, by = "group"), "ggplot")
})

test_that("QC functions accept plain SummarizedExperiment objects", {
  x <- plain_se(simple_glycomic_se())

  expect_s3_class(plot_missing_bar(x), "ggplot")
  expect_s3_class(plot_tic_bar(x), "ggplot")
  expect_s3_class(plot_rank_abundance(x), "ggplot")
  expect_s3_class(plot_int_boxplot(x, by = "group"), "ggplot")
  expect_s3_class(plot_rle(x, by = "group"), "ggplot")
  expect_s3_class(plot_cv_dent(x, by = "group"), "ggplot")
})

test_that("legacy experiment inputs still return experiments", {
  x <- legacy_exp(4, 6)

  expect_s3_class(normalize_median(x), "glyexp_experiment")
  expect_s3_class(suppressMessages(remove_rare(x)), "glyexp_experiment")
  expect_s3_class(transform_clr(x, gamma = 0), "glyexp_experiment")
})
