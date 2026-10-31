coda_site_exp <- function(mat, protein, protein_site) {
  if (is.null(rownames(mat))) {
    rownames(mat) <- paste0("V", seq_len(nrow(mat)))
  }
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  test_glycoproteomic_se(
    mat,
    sample_info = tibble::tibble(
      sample = colnames(mat),
      group = factor(rep(c("A", "B"), c(2, ncol(mat) - 2)))
    ),
    var_info = tibble::tibble(
      variable = rownames(mat),
      protein = protein,
      protein_site = as.integer(protein_site)
    )
  )
}

coda_alr_site_matrix <- function() {
  rbind(
    c(8, 8, 8, 8, 8),
    c(16, 16, 32, 32, 32),
    c(4, 4, 2, 2, 2),
    c(2, 2, 8, 8, 8),
    c(32, 32, 16, 16, 16)
  )
}

test_that("CLR centers by both protein and site while preserving row order", {
  mat <- outer(c(4, 100, 9, 16, 400, 81), c(1, 2, 4, 8))
  x <- coda_site_exp(
    mat,
    protein = c("P1", "P1", "P2", "P1", "P1", "P2"),
    protein_site = c(10, 20, 10, 10, 20, 10)
  )
  S4Vectors::metadata(x)$custom <- "preserved"
  SummarizedExperiment::assayNames(x) <- "signal"
  result <- transform_clr(x, gamma = 0)
  expected <- matrix(c(0.5, 0.5, 1 / 3, 2, 2, 3), nrow = 6, ncol = 4)

  expect_equal(unname(SummarizedExperiment::assay(result)), expected)
  expect_s4_class(result, "GlycoproteomicSE")
  expect_true(methods::validObject(result))
  expect_identical(rownames(result), rownames(x))
  expect_identical(SummarizedExperiment::assayNames(result), "signal")
  expect_identical(
    SummarizedExperiment::rowData(result),
    SummarizedExperiment::rowData(x)
  )
  expect_identical(
    SummarizedExperiment::colData(result),
    SummarizedExperiment::colData(x)
  )
  meta <- S4Vectors::metadata(result)
  expect_identical(meta$custom, "preserved")
  expect_identical(meta$coda_transform, "clr")
  expect_null(meta$coda_reference)
  expect_equal(
    meta$coda_site_transforms,
    tibble::tibble(
      protein = c("P1", "P1", "P2"),
      protein_site = c(10L, 20L, 10L),
      transform = rep("clr", 3),
      reference = rep(NA_character_, 3)
    )
  )

  SummarizedExperiment::assay(x)[c(2, 5), ] <- c(1e6, 1e9)
  changed <- transform_clr(x, gamma = 0)
  expect_equal(
    SummarizedExperiment::assay(changed)[c(1, 3, 4, 6), ],
    SummarizedExperiment::assay(result)[c(1, 3, 4, 6), ]
  )
})

test_that("site-wise ALR removes one reference per successful site", {
  block <- coda_alr_site_matrix()
  mat <- rbind(block, block * 16, c(4, 0, 8, 16, 0))
  x <- coda_site_exp(
    mat,
    protein = rep(c("P1", "P2", "P3"), c(5, 5, 1)),
    protein_site = rep(10, 11)
  )
  x <- x[c(1, 6, 11, 2, 7, 3, 8, 4, 9, 5, 10), ]

  expect_warning(
    result <- transform_alr(x, by = "group", gamma = 0),
    "at least two glycans"
  )

  retained <- !rownames(x) %in% c("V1", "V6")
  expected <- SummarizedExperiment::assay(x)[retained, , drop = FALSE]
  expected[grep("^V[2-5]$", rownames(expected)), ] <- block[-1, ] / 8
  expected[grep("^V([7-9]|10)$", rownames(expected)), ] <- block[-1, ] / 8
  expected["V11", ] <- c(1, 0, 1, 1, 0)
  expect_equal(SummarizedExperiment::assay(result), expected)
  expect_s4_class(result, "GlycoproteomicSE")
  expect_true(methods::validObject(result))
  expect_identical(rownames(result), rownames(x)[retained])
  expect_identical(
    SummarizedExperiment::rowData(result),
    SummarizedExperiment::rowData(x)[retained, , drop = FALSE]
  )
  expect_identical(
    SummarizedExperiment::colData(result),
    SummarizedExperiment::colData(x)
  )
  meta <- S4Vectors::metadata(result)
  expect_identical(meta$coda_transform, "mixed")
  expect_identical(meta$coda_reference, c("V1", "V6"))
  expect_identical(meta$coda_site_transforms$transform, c("alr", "alr", "clr"))
  expect_identical(
    meta$coda_site_transforms$reference,
    c("V1", "V6", NA_character_)
  )
})

test_that("auto_coda selects its method at the 50-feature site boundary", {
  block <- coda_alr_site_matrix()
  small <- rbind(block[1, ], block[rep(2:5, length.out = 49), ])
  large <- rbind(block[1, ], block[rep(2:5, length.out = 50), ])
  x <- coda_site_exp(
    rbind(small, large, c(4, 4, 4, 4, 4)),
    protein = rep("P1", 102),
    protein_site = rep(c(10, 20, 30), c(50, 51, 1))
  )

  result <- suppressMessages(auto_coda(x, by = "group", gamma = 0))

  expected <- rbind(
    sweep(small, 2, exp(colMeans(log(small))), "/"),
    large[-1, ] / 8,
    rep(1, 5)
  )
  expect_equal(unname(SummarizedExperiment::assay(result)), expected)
  expect_identical(rownames(result), rownames(x)[-51])
  meta <- S4Vectors::metadata(result)
  expect_identical(meta$coda_transform, "mixed")
  expect_identical(meta$coda_reference, "V51")
  expect_identical(meta$coda_site_transforms$transform, c("clr", "alr", "clr"))
})

test_that("singleton sites and all-zero site samples keep zeros", {
  mat <- rbind(c(0, 0, 0, 0), c(4, 0, 16, 0), c(16, 0, 4, 0))
  x <- coda_site_exp(mat, rep("P1", 3), c(10, 20, 20))
  expected <- rbind(c(0, 0, 0, 0), c(0.5, 0, 2, 0), c(2, 0, 0.5, 0))

  for (fn in list(transform_clr, transform_alr, auto_coda)) {
    result <- suppressMessages(suppressWarnings(fn(x, gamma = 0)))
    expect_equal(unname(SummarizedExperiment::assay(result)), expected)
    expect_true(all(is.finite(SummarizedExperiment::assay(result))))
  }
  noisy <- transform_clr(x)
  expect_identical(
    unname(SummarizedExperiment::assay(noisy)[mat == 0]),
    rep(0, sum(mat == 0))
  )
})

test_that("site-wise stochastic transforms preserve seed and sample parameters", {
  mat <- rbind(coda_alr_site_matrix(), coda_alr_site_matrix() * 16)
  x <- coda_site_exp(mat, rep(c("P1", "P2"), each = 5), rep(10, 10))
  withr::local_seed(97)
  original_seed <- .Random.seed

  for (fn in list(transform_clr, transform_alr, auto_coda)) {
    first <- suppressMessages(fn(x, by = "group", group_scales = 2, seed = 17))
    second <- suppressMessages(fn(
      x,
      by = SummarizedExperiment::colData(x)$group,
      group_scales = 2,
      seed = 17
    ))
    other_seed <- suppressMessages(fn(
      x,
      by = "group",
      group_scales = 2,
      seed = 18
    ))
    expect_identical(first, second)
    expect_false(identical(
      SummarizedExperiment::assay(first),
      SummarizedExperiment::assay(other_seed)
    ))
    expect_identical(.Random.seed, original_seed)
  }
})

test_that("legacy glycoproteomics experiments use the same site-wise transform", {
  withr::local_options(lifecycle_verbosity = "quiet")
  x <- coda_site_exp(
    rbind(coda_alr_site_matrix(), coda_alr_site_matrix() * 16),
    rep(c("P1", "P2"), each = 5),
    rep(10, 10)
  )
  legacy <- glyexp::experiment(
    SummarizedExperiment::assay(x),
    sample_info = .get_sample_info(x),
    var_info = .get_var_info(x),
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  for (fn in list(transform_clr, transform_alr, auto_coda)) {
    result <- suppressMessages(fn(legacy, by = "group", gamma = 0))
    expected <- suppressMessages(fn(x, by = "group", gamma = 0))
    expect_s3_class(result, "glyexp_experiment")
    expect_equal(result$expr_mat, SummarizedExperiment::assay(expected))
    expect_equal(
      result$meta_data$coda_site_transforms,
      S4Vectors::metadata(expected)$coda_site_transforms
    )
  }
})

test_that("site-wise CoDA rejects incomplete site identities", {
  x <- coda_site_exp(matrix(1:12, nrow = 3), rep("P1", 3), c(10, 20, 20))
  for (column in c("protein", "protein_site")) {
    missing <- x
    SummarizedExperiment::rowData(missing)[[column]][1] <- NA
    absent <- x
    SummarizedExperiment::rowData(absent)[[column]] <- NULL
    for (fn in list(transform_clr, transform_alr, auto_coda)) {
      expect_error(
        suppressMessages(fn(missing)),
        "identifiers cannot be missing"
      )
      expect_error(
        suppressMessages(fn(absent)),
        "requires.*protein.*protein_site"
      )
    }
  }
})

test_that("plain SummarizedExperiment retains whole-matrix CoDA", {
  x <- coda_site_exp(
    outer(c(4, 16, 64, 256), c(1, 2, 4, 8)),
    rep("P1", 4),
    c(10, 10, 20, 20)
  )
  x <- plain_se(x)
  S4Vectors::metadata(x)$exp_type <- "glycoproteomics"

  result <- transform_clr(x, gamma = 0)

  expected <- matrix(c(0.125, 0.5, 2, 8), nrow = 4, ncol = 4)
  expect_equal(unname(SummarizedExperiment::assay(result)), expected)
  expect_identical(as.character(class(result)), "SummarizedExperiment")
  expect_null(S4Vectors::metadata(result)$coda_site_transforms)
})
