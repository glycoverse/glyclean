log_test_se <- function(mat, scale = "raw") {
  SummarizedExperiment::SummarizedExperiment(
    assays = list(signal = mat),
    metadata = list(scale = scale, custom = "keep")
  )
}

test_that("ordinary scales convert directly to every supported base", {
  raw <- matrix(c(0, NA, 0.25, 1, 4, 100), 2)
  for (scale in c("raw", "log", "log2", "log10")) {
    mat <- switch(
      scale,
      raw = raw,
      log = log(raw),
      log2 = log2(raw),
      log10 = log10(raw)
    )
    for (base in c(2, exp(1), 10)) {
      x <- log_test_se(mat, scale)
      before <- x
      result <- transform_log(x, base)
      expect_equal(SummarizedExperiment::assay(result), log(raw, base))
      expect_identical(
        .get_scale(result),
        c("log2", "log", "log10")[[match(base, c(2, exp(1), 10))]]
      )
      expect_identical(x, before)
      expect_identical(SummarizedExperiment::assayNames(result), "signal")
      expect_identical(S4Vectors::metadata(result)$custom, "keep")
      expect_identical(apply_pipeline(x, get_pipeline(result)), result)
    }
  }
})

test_that("extreme logs avoid exponentiation and matching scales stay exact", {
  mat <- matrix(c(-1e300, 1e300, -Inf, NA), 2)
  x <- log_test_se(mat, "log10")
  expect_identical(SummarizedExperiment::assay(transform_log(x, 10)), mat)
  expect_equal(SummarizedExperiment::assay(transform_log(x)), mat * log2(10))
  expect_error(
    transform_log(log_test_se(
      matrix(c(-1, 1) * .Machine$double.xmax),
      "log10"
    )),
    "numeric range"
  )
  S4Vectors::metadata(x)$scale <- NULL
  SummarizedExperiment::assay(x) <- matrix(1:4, 2)
  expect_equal(
    SummarizedExperiment::assay(transform_log(x)),
    log2(matrix(1:4, 2))
  )
})

test_that("invalid scales, bases, and values fail without modifying input", {
  for (base in list(NULL, NA, NaN, Inf, -2, 0, 1, 3, "2", c(2, 10))) {
    expect_error(transform_log(log_test_se(matrix(1)), base))
  }
  for (scale in list("logx", "bad", NULL, NA, c("raw", "log2"))) {
    expect_error(transform_log(log_test_se(matrix(1), scale)))
  }
  for (scale in c("raw", "log", "log2", "log10")) {
    for (value in c(NaN, Inf)) {
      x <- log_test_se(matrix(value), scale)
      before <- x
      expect_error(transform_log(x), "NaN or positive infinity")
      expect_identical(x, before)
      expect_length(get_pipeline(x), 0)
    }
  }
  for (value in c(-1, -Inf)) {
    expect_error(transform_log(log_test_se(matrix(value))), "non-negative")
  }
  expect_error(transform_log(matrix(1)), "Must inherit")
})

test_that("containers preserve annotations and append replayable steps", {
  containers <- list(
    simple_exp(3, 2),
    test_glycoproteomic_se(matrix(
      1:6,
      3,
      dimnames = list(paste0("V", 1:3), paste0("S", 1:2))
    )),
    legacy_exp(3, 2),
    legacy_exp(3, 2, "traitproteomics", "N")
  )
  for (x in containers) {
    before <- zero_to_na(x)
    result <- transform_log(before)
    expect_identical(class(result), class(x))
    expect_identical(.get_sample_info(result), .get_sample_info(x))
    expect_identical(.get_var_info(result), .get_var_info(x))
    history <- get_pipeline(result)
    expect_length(history, 2)
    expect_identical(history[[1]], get_pipeline(before)[[1]])
    expect_identical(history[[2]]$parameters, list(base = 2))
    expect_identical(apply_pipeline(x, history), result)
    expect_identical(
      apply_pipeline(x, glyclean_pipeline(zero_to_na(), transform_log())),
      result
    )
    expect_identical(
      .pipeline_operation_name(history[[2]][["function"]]),
      "transform_log"
    )
  }
})

test_that("sparse, empty, singleton, and missing assays retain dimensions", {
  matrices <- list(matrix(0), matrix(NA_real_, 2, 3), matrix(numeric(), 0, 2))
  if (requireNamespace("Matrix", quietly = TRUE)) {
    matrices <- c(
      matrices,
      list(Matrix::Matrix(matrix(c(0, 1, NA, 4), 2), sparse = TRUE))
    )
  }
  for (mat in matrices) {
    result <- transform_log(log_test_se(mat))
    expect_equal(
      as.matrix(SummarizedExperiment::assay(result)),
      log2(as.matrix(mat))
    )
    expect_identical(dim(result), dim(mat))
  }
})
