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
  expected <- matrix(c(-1, -1, -log2(3), 1, 1, log2(3)), nrow = 6, ncol = 4)

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
  expect_identical(meta$scale, "logx")
  expect_identical(meta$custom, "preserved")
  expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                     names(meta)))
  expect_identical(apply_pipeline(x, get_pipeline(result)), result)

  SummarizedExperiment::assay(x)[c(2, 5), ] <- c(1e6, 1e9)
  changed <- transform_clr(x, gamma = 0)
  expect_equal(
    SummarizedExperiment::assay(changed)[c(1, 3, 4, 6), ],
    SummarizedExperiment::assay(result)[c(1, 3, 4, 6), ]
  )
})

test_that("ALR explicitly rejects glycoproteomics without consuming RNG", {
  x <- coda_site_exp(coda_alr_site_matrix(), rep("P1", 5), rep(10, 5))
  withr::local_seed(97)
  original_seed <- .Random.seed
  expect_error(
    transform_alr(x, by = "group"),
    "does not support glycoproteomics"
  )
  expect_identical(.Random.seed, original_seed)

  withr::local_options(lifecycle_verbosity = "quiet")
  legacy <- glyexp::experiment(
    SummarizedExperiment::assay(x),
    sample_info = .get_sample_info(x),
    var_info = .get_var_info(x),
    exp_type = "glycoproteomics", glycan_type = "N"
  )
  expect_error(transform_alr(legacy), "does not support glycoproteomics")
  expect_identical(.Random.seed, original_seed)

  rm(".Random.seed", envir = .GlobalEnv)
  expect_error(transform_alr(x), "does not support glycoproteomics")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("auto_coda uses CLR at every site regardless of the 50-feature boundary", {
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
    sweep(log2(small), 2, colMeans(log2(small)), "-"),
    sweep(log2(large), 2, colMeans(log2(large)), "-"),
    rep(0, 5)
  )
  expect_equal(unname(SummarizedExperiment::assay(result)), expected)
  expect_identical(rownames(result), rownames(x))
  expect_identical(result, transform_clr(x, by = "group", gamma = 0))
  meta <- S4Vectors::metadata(result)
  expect_identical(meta$scale, "logx")
  expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                     names(meta)))
  expect_identical(apply_pipeline(x, get_pipeline(result)), result)
})

test_that("singleton sites and all-zero site samples reject zeros", {
  mat <- rbind(c(0, 0, 0, 0), c(4, 0, 16, 0), c(16, 0, 4, 0))
  x <- coda_site_exp(mat, rep("P1", 3), c(10, 20, 20))
  for (fn in list(transform_clr, auto_coda)) {
    expect_error(suppressMessages(fn(x, gamma = 0)), "Zero values")
    expect_error(suppressMessages(fn(x)), "zero_to_na")
  }
})

test_that("site-wise stochastic transforms preserve seed and sample parameters", {
  mat <- rbind(coda_alr_site_matrix(), coda_alr_site_matrix() * 16)
  x <- coda_site_exp(mat, rep(c("P1", "P2"), each = 5), rep(10, 10))
  withr::local_seed(97)
  original_seed <- .Random.seed

  for (fn in list(transform_clr, auto_coda)) {
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
    # Both inputs use the same grouping values, while only the column-name
    # input retains that name for printing its recorded history.
    metadata <- S4Vectors::metadata(first)
    expect_identical(attr(metadata$pipeline[[1]], "by_column"), "group")
    expect_null(attr(get_pipeline(second)[[1]], "by_column"))
    attr(metadata$pipeline[[1]], "by_column") <- NULL
    expected <- first
    S4Vectors::metadata(expected) <- metadata
    expect_identical(expected, second)
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
  for (fn in list(transform_clr, auto_coda)) {
    result <- suppressMessages(fn(legacy, by = "group", gamma = 0))
    expected <- suppressMessages(fn(x, by = "group", gamma = 0))
    expect_s3_class(result, "glyexp_experiment")
    expect_equal(result$expr_mat, SummarizedExperiment::assay(expected))
    expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                       names(result$meta_data)))
    expect_identical(apply_pipeline(legacy, get_pipeline(result)), result)
  }
})

test_that("site-wise CoDA rejects incomplete site identities", {
  x <- coda_site_exp(matrix(1:12, nrow = 3), rep("P1", 3), c(10, 20, 20))
  for (column in c("protein", "protein_site")) {
    missing <- x
    SummarizedExperiment::rowData(missing)[[column]][1] <- NA
    absent <- x
    SummarizedExperiment::rowData(absent)[[column]] <- NULL
    for (fn in list(transform_clr, auto_coda)) {
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

  expected <- matrix(c(-3, -1, 1, 3), nrow = 4, ncol = 4)
  expect_equal(unname(SummarizedExperiment::assay(result)), expected)
  expect_identical(as.character(class(result)), "SummarizedExperiment")
  expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                     names(S4Vectors::metadata(result))))
  expect_identical(apply_pipeline(x, get_pipeline(result)), result)
})


test_that("auto_coda is identical to explicit CLR across external RNG states", {
  block <- coda_alr_site_matrix()
  mat <- rbind(block[rep(1:5, length.out = 51), ], block)
  x <- coda_site_exp(mat, rep(c("P1", "P2"), c(51, 5)), rep(10, 56))
  x <- x[c(1, 52, 2:51, 53:56), ]
  withr::local_seed(99)
  expected <- transform_clr(x, by = "group", group_scales = 2, seed = 17)
  for (outside_seed in c(42, 381)) {
    set.seed(outside_seed)
    invisible(runif(7))
    caller <- .Random.seed
    result <- suppressMessages(auto_coda(
      x, by = "group", group_scales = 2, seed = 17
    ))
    expect_identical(result, expected)
    expect_identical(.Random.seed, caller)
  }
  rm(".Random.seed", envir = .GlobalEnv)
  expect_identical(
    suppressMessages(auto_coda(x, by = "group", group_scales = 2, seed = 17)),
    expected
  )
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
