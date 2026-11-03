seed_vsn_exp <- function() {
  n <- 3000L
  values <- exp(matrix(
    6 + sin(seq_len(n * 6L) * 0.17) + cos(seq_len(n * 6L) * 0.013),
    nrow = n
  ))
  dimnames(values) <- list(paste0("V", seq_len(n)), paste0("S", 1:6))
  SummarizedExperiment::SummarizedExperiment(assays = list(signal = values))
}

seed_without_global_state <- function(code) {
  kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  if (had_seed) rm(".Random.seed", envir = .GlobalEnv)
  force(code)
}

test_that("actual VSN subsampling is controlled by its seed", {
  skip_if_not_installed("vsn")
  x <- seed_vsn_exp()
  withr::local_seed(92)
  state <- .Random.seed
  kind <- RNGkind()
  first <- normalize_vsn(x, subsample = 1000, seed = 17)
  expect_identical(.Random.seed, state)
  expect_identical(RNGkind(), kind)

  set.seed(981)
  state <- .Random.seed
  second <- normalize_vsn(x, subsample = 1000, seed = 17)
  expect_identical(.Random.seed, state)
  expect_identical(SummarizedExperiment::assay(second),
                   SummarizedExperiment::assay(first))
  different <- normalize_vsn(x, subsample = 1000, seed = 28)
  expect_false(identical(SummarizedExperiment::assay(different),
                         SummarizedExperiment::assay(first)))
  expect_identical(SummarizedExperiment::assayNames(first), "signal")
  expect_identical(x, seed_vsn_exp())
})

test_that("VSN restores caller RNG on errors and with no existing seed", {
  skip_if_not_installed("vsn")
  x <- seed_vsn_exp()
  withr::local_seed(82)
  RNGkind("L'Ecuyer-CMRG")
  state <- .Random.seed
  kind <- RNGkind()
  expect_error(normalize_vsn(x[1:10, ], seed = 17), "at least 42")
  expect_identical(.Random.seed, state)
  expect_identical(RNGkind(), kind)
  seed_without_global_state({
    normalize_vsn(x, subsample = 1000, seed = 17)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_error(normalize_vsn(x[1:10, ], seed = 17), "at least 42")
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
})

test_that("stochastic aggregation is reproducible and preserves caller RNG", {
  x <- simple_exp(8, 4)
  original <- x
  f <- function(values) sum(values) + stats::runif(1)
  withr::local_seed(61)
  state <- .Random.seed
  kind <- RNGkind()
  first <- aggregate(x, f = f, seed = 37, standardize_variable = FALSE)
  expect_identical(.Random.seed, state)
  expect_identical(RNGkind(), kind)
  set.seed(471)
  state <- .Random.seed
  second <- aggregate(x, f = f, seed = 37, standardize_variable = FALSE)
  expect_identical(.Random.seed, state)
  expect_identical(SummarizedExperiment::assay(first),
                   SummarizedExperiment::assay(second))
  different <- aggregate(x, f = f, seed = 38, standardize_variable = FALSE)
  expect_false(identical(SummarizedExperiment::assay(first),
                         SummarizedExperiment::assay(different)))
  expect_identical(x, original)
  expect_identical(SummarizedExperiment::colData(first),
                   SummarizedExperiment::colData(x))
})

test_that("aggregation restores RNG after callback errors and absent state", {
  x <- simple_exp(4, 3)
  f <- function(values) {
    stats::runif(3)
    stop("callback failed")
  }
  withr::local_seed(21)
  RNGkind("L'Ecuyer-CMRG")
  state <- .Random.seed
  kind <- RNGkind()
  expect_error(aggregate(x, f = f, seed = 25, standardize_variable = FALSE),
               "callback failed")
  expect_identical(.Random.seed, state)
  expect_identical(RNGkind(), kind)
  seed_without_global_state({
    aggregate(x, f = function(values) sum(values) + stats::runif(1),
              seed = 25, standardize_variable = FALSE)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_error(aggregate(x, f = f, seed = 25, standardize_variable = FALSE),
                 "callback failed")
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
})

test_that("aggregation defaults remain deterministic and validate seeds", {
  x <- simple_exp(4, 3)
  first <- aggregate(x, seed = 7, standardize_variable = FALSE)
  second <- aggregate(x, seed = 19, standardize_variable = FALSE)
  expect_identical(get_pipeline(first)[[1]]$parameters$seed, 7)
  expect_identical(get_pipeline(second)[[1]]$parameters$seed, 19)
  S4Vectors::metadata(first)$pipeline <- NULL
  S4Vectors::metadata(second)$pipeline <- NULL
  expect_identical(first, second)
  expect_error(aggregate(x, seed = -1, standardize_variable = FALSE))
  expect_error(aggregate(x, seed = NA_integer_, standardize_variable = FALSE))
  expect_error(normalize_vsn(seed_vsn_exp(), seed = -1))
})

test_that("deprecated automatic aggregation forwards the seed", {
  x <- simple_exp(4, 3)
  observed <- NULL
  local_mocked_bindings(aggregate = function(exp, standardize_variable = TRUE,
                                             seed = 123, ...) {
    observed <<- list(seed = seed, standardize_variable = standardize_variable)
    exp
  })
  result <- suppressWarnings(auto_aggregate(x, standardize_variable = FALSE,
                                            seed = 73))
  expect_identical(result, x)
  expect_identical(observed, list(seed = 73, standardize_variable = FALSE))
})

test_that("auto_clean forwards its seed to aggregation for both experiment types", {
  observed <- numeric()
  local_mocked_bindings(
    auto_remove = function(exp, ...) exp,
    auto_normalize = function(exp, ...) exp,
    auto_impute = function(exp, ...) exp,
    auto_correct_batch_effect = function(exp, ...) exp,
    aggregate = function(exp, standardize_variable = TRUE, seed = 123, ...) {
      observed <<- c(observed, seed)
      exp
    }
  )
  comic <- simple_exp(4, 3)
  proteomic <- simple_glycoproteomic_exp(4, 3)
  expect_identical(suppressMessages(auto_clean(comic, seed = 87)), comic)
  expect_identical(suppressMessages(auto_clean(proteomic, seed = 87)), proteomic)
  expect_identical(observed, c(87, 87))
})

test_that("aggregation restores RNG kind even when an unseeded callback changes it", {
  x <- simple_exp(4, 3)
  for (fail in c(FALSE, TRUE)) {
    f <- function(values) {
      RNGkind("L'Ecuyer-CMRG")
      if (fail) stop("changed kind")
      sum(values) + stats::runif(1)
    }
    withr::local_seed(21)
    state <- .Random.seed
    kind <- RNGkind()
    if (fail) {
      expect_error(aggregate(x, f = f, standardize_variable = FALSE), "changed kind")
    } else {
      aggregate(x, f = f, standardize_variable = FALSE)
    }
    expect_identical(.Random.seed, state)
    expect_identical(RNGkind(), kind)
    seed_without_global_state({
      if (fail) {
        expect_error(aggregate(x, f = f, standardize_variable = FALSE), "changed kind")
      } else {
        aggregate(x, f = f, standardize_variable = FALSE)
      }
      expect_identical(RNGkind(), kind)
      expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    })
  }
})
