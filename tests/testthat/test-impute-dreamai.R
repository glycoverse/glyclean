test_that("DreamAI imputes each group in its working domain and preserves observed values", {
  local_mocked_bindings(.dreamai_ensemble = function(data, method, seed, args) {
    expect_identical(method, c("KNN", "MissForest"))
    expect_equal(seed, 42)
    expect_identical(args, list(k = 2))
    matrix(mean(data[1, ]), nrow(data), ncol(data))
  })
  work <- rbind(
    c(-0.8, 0, 1.3, 0.7, 1, -1),
    c(NA, 1, 0.4, 0.8, 2, 0.7),
    c(1.2, NA, 0.5, 0.9, 3, 0.3)
  )
  colnames(work) <- rep(c("A", "B"), 3)
  expected <- work
  expected[2, 1] <- mean(work[1, c(1, 3, 5)])
  expected[3, 2] <- mean(work[1, c(2, 4, 6)])
  groups <- factor(rep(c("first", "second"), 3))
  scales <- list(
    raw = \(z) 2^z,
    log = \(z) z * log(2),
    log2 = identity,
    log10 = \(z) z * log10(2),
    logx = identity
  )
  pipeline <- glyclean_pipeline(impute_dreamai(
    by = "group",
    method = c("KNN", "MissForest"),
    k = 2,
    seed = 42
  ))
  for (scale in names(scales)) {
    values <- scales[[scale]](work)
    x <- SummarizedExperiment::SummarizedExperiment(
      assays = list(signal = values, original = values),
      colData = S4Vectors::DataFrame(group = groups),
      metadata = list(scale = scale)
    )
    result <- apply_pipeline(x, pipeline)
    assay <- SummarizedExperiment::assay(result)
    expect_equal(assay, scales[[scale]](expected))
    expect_identical(assay[!is.na(values)], values[!is.na(values)])
    expect_identical(SummarizedExperiment::assay(result, "original"), values)
    expect_identical(
      SummarizedExperiment::colData(result),
      SummarizedExperiment::colData(x)
    )
    expect_identical(.get_scale(result), scale)
    expect_identical(apply_pipeline(x, get_pipeline(result)), result)
  }
})

test_that("DreamAI skips complete inputs and validates its arguments", {
  x <- simple_glycomic_se()
  complete <- impute_dreamai(x)
  expect_s4_class(complete, "GlycomicSE")
  expect_identical(
    SummarizedExperiment::assay(complete),
    SummarizedExperiment::assay(x)
  )
  expect_length(get_pipeline(complete), 1L)
  expect_error(impute_dreamai(x, seed = -1), "seed")
  expect_error(impute_dreamai(x, method = "unknown"), "method")
  expect_error(impute_dreamai(x, out = "KNN"), "method")
  expect_error(impute_dreamai(x, NULL, "KNN", 2), "by name")
})

test_that("missing DreamAI gives manual installation instructions even for a small fit", {
  skip_if(rlang::is_installed("DreamAI"))
  x <- simple_glycomic_se()
  SummarizedExperiment::assay(x)[1, 1] <- NA_real_
  err <- expect_error(
    impute_dreamai(x),
    class = "glyclean_dreamai_not_installed"
  )
  expect_match(conditionMessage(err), "install_github", fixed = TRUE)
  expect_match(conditionMessage(err), "WangLab-MSSM/DreamAI/Code", fixed = TRUE)
})

test_that("DreamAI checks component limits on each fit", {
  values <- matrix(seq_len(30), 5)
  values[2, -1] <- NA_integer_
  expect_error(
    .check_dreamai_input(values, "KNN", list()),
    "Row 2 has only one"
  )
  values[2, 1] <- NA_integer_
  expect_error(
    .check_dreamai_input(values, "KNN", list()),
    "one observed value"
  )
  values <- matrix(seq_len(30), 5)
  values[, 3] <- NA_integer_
  expect_error(
    .check_dreamai_input(values, "Birnn", list()),
    "one observed value"
  )
  values <- matrix(seq_len(30), 5)
  values[1, 1] <- NA_integer_
  expect_error(
    .check_dreamai_input(values, "SpectroFM", list()),
    "101 observed and 11 missing"
  )
  expect_error(
    .check_dreamai_input(values[, 1:2], "RegImpute", list()),
    "three samples"
  )
  expect_error(.check_dreamai_input(values, "KNN", list(k = 5)), "k")
  expect_silent(.check_dreamai_input(values, "KNN", list(k = 2)))
})

test_that("the installed DreamAI package fills a known intensity with KNN", {
  skip_if_not(rlang::is_installed(c("DreamAI", "callr", "devtools", "impute")))
  values <- matrix(c(2, 3, 5, 8, 4, 6, NA, 16, 8, 12, 20, 32), nrow = 4)
  x <- test_glycomic_se(values)
  result <- impute_dreamai(x, method = "KNN", k = 2)
  assay <- SummarizedExperiment::assay(result)
  expect_equal(unname(assay[3, 2]), 10)
  expect_identical(assay[!is.na(values)], values[!is.na(values)])
})

test_that("the six-method DreamAI ensemble replays without changing the caller's session", {
  skip_if_not(rlang::is_installed(c("DreamAI", "callr", "devtools", "impute")))
  withr::local_dir(withr::local_tempdir())
  writeLines("original", "NULL")
  withr::local_seed(436)
  values <- matrix(2^rnorm(60 * 16, 12, 1.1), 60)
  values[sample(length(values), 100)] <- NA_real_
  x <- SummarizedExperiment::SummarizedExperiment(
    assays = list(signal = values)
  )
  state <- .Random.seed
  warn <- getOption("warn")
  sinks <- sink.number()
  packages <- search()

  result <- impute_dreamai(x, ntree = 10, maxiter_ADMIN = 2, iter_SpectroFM = 5)
  assay <- SummarizedExperiment::assay(result)
  expect_true(all(is.finite(assay)))
  expect_identical(assay[!is.na(values)], values[!is.na(values)])
  expect_identical(apply_pipeline(x, get_pipeline(result)), result)
  expect_identical(.Random.seed, state)
  expect_identical(getOption("warn"), warn)
  expect_identical(sink.number(), sinks)
  expect_identical(search(), packages)
  expect_identical(readLines("NULL"), "original")
})
