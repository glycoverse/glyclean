zero_test_se <- function(mat) {
  dimnames(mat) <- list(paste0("V", seq_len(nrow(mat))),
                        paste0("S", seq_len(ncol(mat))))
  SummarizedExperiment::SummarizedExperiment(
    assays = list(signal = mat),
    rowData = S4Vectors::DataFrame(feature = seq_len(nrow(mat))),
    colData = S4Vectors::DataFrame(group = rep("A", ncol(mat))),
    metadata = list(custom = list(keep = TRUE), scale = "raw",
                    coda_reference = "leave untouched")
  )
}

test_that("only exact zeros become NA, including negative zero", {
  mat <- matrix(c(0, -0, NA_real_, NaN, 1, -2, 1e-12, Inf, -Inf), 3)
  x <- zero_test_se(mat)
  before <- x
  result <- zero_to_na(x)
  expected <- SummarizedExperiment::assay(x)
  expected[1:2] <- NA_real_
  expect_identical(SummarizedExperiment::assay(result), expected)
  expect_identical(x, before)
  expect_identical(SummarizedExperiment::rowData(result),
                   SummarizedExperiment::rowData(x))
  expect_identical(SummarizedExperiment::colData(result),
                   SummarizedExperiment::colData(x))
  expect_identical(SummarizedExperiment::assayNames(result), "signal")
  meta <- S4Vectors::metadata(result)
  meta$pipeline <- NULL
  expect_identical(meta, S4Vectors::metadata(x))
})

test_that("no-zero, all-zero and integer assays retain their structure", {
  for (mat in list(matrix(1:6, 2), matrix(0L, 2, 3),
                   matrix(c(0L, 1L, NA_integer_, -2L), 2))) {
    x <- zero_test_se(mat)
    expected <- SummarizedExperiment::assay(x)
    expected[!is.na(expected) & expected == 0] <- NA
    result <- zero_to_na(x)
    expect_identical(SummarizedExperiment::assay(result), expected)
    expect_type(SummarizedExperiment::assay(result), "integer")
  }
})

test_that("all supported containers record and replay zero conversion", {
  withr::local_options(lifecycle_verbosity = "quiet")
  containers <- list(
    zero_test_se(matrix(c(0, 1, NA, 3, 0, 5), 3)),
    simple_exp(3, 2),
    test_glycoproteomic_se(matrix(1:6, 3,
      dimnames = list(paste0("V", 1:3), paste0("S", 1:2)))),
    legacy_exp(3, 2),
    legacy_exp(3, 2, exp_type = "traitproteomics", glycan_type = "N")
  )
  for (x in containers) {
    if (inherits(x, "glyexp_experiment")) {
      x$expr_mat[1] <- 0
      x$meta_data$custom <- "preserve"
    } else {
      SummarizedExperiment::assay(x)[1] <- 0
      S4Vectors::metadata(x)$custom <- "preserve"
    }
    converted <- zero_to_na(x)
    history <- get_pipeline(converted)
    expect_length(history, 1L)
    expect_identical(history[[1]][["function"]], zero_to_na)
    expect_identical(history[[1]]$parameters, list())
    expect_identical(class(converted), class(x))
    expect_identical(apply_pipeline(x, history), converted)
    manual <- impute_zero(converted)
    expect_identical(apply_pipeline(x, get_pipeline(manual)), manual)
    expect_identical(apply_pipeline(x, glyclean_pipeline(zero_to_na(),
                                                        impute_zero())), manual)
    expect_length(get_pipeline(manual), 2L)
  }
})

test_that("sparse assays convert implicit zeros and retain missing entries", {
  skip_if_not_installed("Matrix")
  mat <- Matrix::Matrix(matrix(c(0, 1, NA, 0, -2, 0), 2), sparse = TRUE)
  x <- zero_test_se(mat)
  expected <- as.matrix(SummarizedExperiment::assay(x))
  expected[!is.na(expected) & expected == 0] <- NA
  result <- zero_to_na(x)
  expect_identical(as.matrix(SummarizedExperiment::assay(result)), expected)
  expect_identical(apply_pipeline(x, get_pipeline(result)), result)
})

test_that("invalid inputs fail without producing history", {
  expect_error(zero_to_na(matrix(0, 2, 2)), "Must inherit")
  expect_error(glyclean_pipeline(zero_to_na(unknown = TRUE)))
})

test_that("existing history is appended without rewriting previous steps", {
  x <- zero_test_se(matrix(c(NA, 1, 2, 0), 2))
  previous <- impute_zero(x)
  history <- get_pipeline(previous)
  result <- zero_to_na(previous)
  expect_identical(get_pipeline(result)[[1]], history[[1]])
  expect_length(get_pipeline(result), 2L)
  expect_identical(apply_pipeline(x, get_pipeline(result)), result)
})
