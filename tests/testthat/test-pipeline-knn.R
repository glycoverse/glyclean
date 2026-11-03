pipeline_knn_input <- function() {
  mat <- withr::with_seed(791, matrix(exp(stats::rnorm(40 * 30, 4, 0.8)), 40, 30))
  mat[seq(8, length(mat), by = 37)] <- NA_real_
  dimnames(mat) <- list(paste0("V", seq_len(nrow(mat))),
                        paste0("S", seq_len(ncol(mat))))
  SummarizedExperiment::SummarizedExperiment(assays = list(signal = mat))
}

pipeline_knn_quiet <- function(fun, ...) {
  invisible(utils::capture.output(result <- fun(...)))
  result
}

test_that("KNN recipes use only the explicit seed", {
  skip_if_not_installed("impute")
  x <- pipeline_knn_input()
  recipes <- list(
    glyclean_pipeline(impute_sw_knn(k = 3, maxp = 12, seed = 99)),
    glyclean_pipeline(impute_fw_knn(k = 3, maxp = 12, seed = 99))
  )
  for (recipe in recipes) {
    expect_identical(recipe[[1]]$parameters$seed, 99)
    expect_false("rng.seed" %in% names(recipe[[1]]$parameters))
  }
  for (recipe in list(
    glyclean_pipeline(impute_sw_knn(rng.seed = 99)),
    glyclean_pipeline(impute_fw_knn(rng.seed = 99, seed = 99))
  )) {
    expect_error(apply_pipeline(x, recipe), "matched by multiple")
  }
})

test_that("KNN recipes execute and record the explicit seed", {
  skip_if_not_installed("impute")
  x <- pipeline_knn_input()
  recipes <- list(
    glyclean_pipeline(impute_sw_knn(k = 3, maxp = 12, seed = 99)),
    glyclean_pipeline(impute_fw_knn(k = 3, maxp = 12, seed = 99))
  )
  functions <- list(impute_sw_knn, impute_fw_knn)
  withr::local_seed(918)
  for (i in seq_along(recipes)) {
    state <- .Random.seed
    result <- pipeline_knn_quiet(apply_pipeline, x, recipes[[i]])
    expect_identical(.Random.seed, state)
    expected <- pipeline_knn_quiet(functions[[i]], x, k = 3, maxp = 12, seed = 99)
    expect_identical(SummarizedExperiment::assay(result),
                     SummarizedExperiment::assay(expected))
    history <- get_pipeline(result)
    expect_length(history, 1L)
    expect_identical(history[[1]]$parameters$seed, 99)
    expect_false("rng.seed" %in% names(history[[1]]$parameters))
  }
})

test_that("direct KNN calls with explicit seeds produce replayable histories", {
  skip_if_not_installed("impute")
  x <- pipeline_knn_input()
  withr::local_seed(387)
  for (fun in list(impute_sw_knn, impute_fw_knn)) {
    results <- list(
      pipeline_knn_quiet(fun, x, k = 3, maxp = 12, seed = 99),
      pipeline_knn_quiet(fun, x, 3, NULL, 0.5, 0.8, 12, seed = 99)
    )
    for (result in results) {
      history <- get_pipeline(result)
      expect_identical(history[[1]]$parameters$seed, 99)
      expect_false("rng.seed" %in% names(history[[1]]$parameters))
      set.seed(941)
      state <- .Random.seed
      replay <- pipeline_knn_quiet(apply_pipeline, x,
                                   unserialize(serialize(history, NULL)))
      expect_identical(.Random.seed, state)
      expect_identical(SummarizedExperiment::assay(replay),
                       SummarizedExperiment::assay(result))
      expect_length(get_pipeline(replay), 1L)
    }
  }
})

test_that("recipe construction and application preserve an absent RNG state", {
  skip_if_not_installed("impute")
  x <- pipeline_knn_input()
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  if (had_seed) rm(".Random.seed", envir = .GlobalEnv)
  recipe <- glyclean_pipeline(impute_sw_knn(k = 3, maxp = 12, seed = 99))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  result <- pipeline_knn_quiet(apply_pipeline, x, recipe)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(RNGkind(), old_kind)
  pipeline_knn_quiet(apply_pipeline, x, get_pipeline(result))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_error(apply_pipeline(x, glyclean_pipeline(impute_sw_knn(rng.seed = 99))),
               "matched by multiple")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
