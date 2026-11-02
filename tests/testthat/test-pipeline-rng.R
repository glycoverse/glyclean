test_that("fresh pipelines use each operation seed independently of ambient RNG", {
  mat <- matrix(seq_len(12), nrow = 3,
                dimnames = list(paste0("V", 1:3), paste0("S", 1:4)))
  x <- test_glycomic_se(mat)
  random_sum <- function(x) sum(x) + stats::runif(1L)
  pipeline <- glyclean_pipeline(
    aggregate(to_level = "g", standardize_variable = FALSE, f = random_sum, seed = 31),
    aggregate(to_level = "g", standardize_variable = FALSE, f = random_sum, seed = 73)
  )
  set.seed(753)
  manual <- aggregate(x, to_level = "g", standardize_variable = FALSE,
                      f = random_sum, seed = 31)
  manual <- aggregate(manual, to_level = "g", standardize_variable = FALSE,
                      f = random_sum, seed = 73)
  set.seed(753)
  before <- .Random.seed
  actual <- apply_pipeline(x, pipeline)
  expect_identical(.Random.seed, before)
  expect_identical(SummarizedExperiment::assay(actual),
                   SummarizedExperiment::assay(manual))
  expect_length(get_pipeline(actual), 2L)
  # A recorded history also ignores a different ambient random state.
  set.seed(92)
  history_before <- .Random.seed
  replay <- apply_pipeline(x, get_pipeline(actual))
  expect_identical(.Random.seed, history_before)
  expect_identical(SummarizedExperiment::assay(replay),
                   SummarizedExperiment::assay(actual))
})

test_that("pipeline errors restore the caller random state", {
  x <- test_glycomic_se(matrix(seq_len(6), nrow = 2,
    dimnames = list(c("V1", "V2"), c("S1", "S2", "S3"))))
  fail_after_draw <- function(x) {
    stats::runif(3L)
    stop("intentional random failure")
  }
  pipeline <- glyclean_pipeline(aggregate(
    to_level = "g", standardize_variable = FALSE, f = fail_after_draw
  ))
  set.seed(826)
  before <- .Random.seed
  expect_error(apply_pipeline(x, pipeline), "intentional random failure")
  expect_identical(.Random.seed, before)
})

test_that("VSN subsampling records the effective seed and replays independently", {
  skip_if_not_installed("vsn")
  set.seed(112)
  mat <- matrix(rexp(160 * 8, rate = 0.01), nrow = 160,
                dimnames = list(paste0("v", 1:160), paste0("s", 1:8)))
  exp <- SummarizedExperiment::SummarizedExperiment(assays = list(signal = mat))
  set.seed(73)
  before <- .Random.seed
  result <- suppressWarnings(normalize_vsn(exp, subsample = 80))
  history <- get_pipeline(result)
  expect_identical(history[[1]]$parameters$seed, 123)
  expect_identical(.Random.seed, before)
  expect_identical(history[[1]]$parameters$subsample, 80)
  set.seed(911)
  outside <- .Random.seed
  replay <- suppressWarnings(apply_pipeline(exp, history))
  expect_identical(SummarizedExperiment::assay(replay),
                   SummarizedExperiment::assay(result))
  expect_identical(.Random.seed, outside)
})
