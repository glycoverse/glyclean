seed_impute_exp <- function() {
  mat <- withr::with_seed(791, matrix(exp(stats::rnorm(40 * 30, 4, 0.8)), 40, 30))
  mat[seq(8, length(mat), by = 37)] <- NA_real_
  rownames(mat) <- paste0("V", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  SummarizedExperiment::SummarizedExperiment(assays = list(signal = mat))
}

quiet_seed_call <- function(fun, ...) {
  invisible(utils::capture.output(result <- fun(...)))
  result
}

test_that("KNN seeds control actual split partitioning and restore caller RNG", {
  skip_if_not_installed("impute")
  x <- seed_impute_exp()
  for (fun in list(impute_sw_knn, impute_fw_knn)) {
    set.seed(901)
    before <- .Random.seed
    first <- quiet_seed_call(fun, x, k = 3, maxp = 12, seed = 37)
    expect_identical(.Random.seed, before)
    set.seed(902)
    before <- .Random.seed
    second <- quiet_seed_call(fun, x, k = 3, maxp = 12, seed = 37)
    expect_identical(.Random.seed, before)
    expect_identical(SummarizedExperiment::assay(first),
                     SummarizedExperiment::assay(second))
    different <- quiet_seed_call(fun, x, k = 3, maxp = 12, seed = 93)
    expect_false(identical(SummarizedExperiment::assay(first),
                           SummarizedExperiment::assay(different)))
    expect_identical(SummarizedExperiment::assayNames(first), "signal")
  }
})

test_that("KNN accepts only the explicit seed and preserves other backend arguments", {
  skip_if_not_installed("impute")
  x <- seed_impute_exp()
  for (fun in list(impute_sw_knn, impute_fw_knn)) {
    expected <- quiet_seed_call(fun, x, k = 3, maxp = 12, seed = 37)
    positional <- quiet_seed_call(fun, x, 3, NULL, 0.5, 0.8, 12, seed = 37)
    expect_identical(SummarizedExperiment::assay(positional),
                     SummarizedExperiment::assay(expected))
    expect_error(fun(x, rng.seed = 37), "matched by multiple")
    expect_error(fun(x, seed = 37, rng.seed = 37), "matched by multiple")
    expect_error(fun(x, 3, NULL, 0.5, 0.8, 12, 37), "unused argument")
  }
})

test_that("BPCA and SVD seed wrappers cover random dependency paths", {
  x <- seed_impute_exp()
  # Their ordinary fit is deterministic; optional CV/dependency paths can draw.
  local_mocked_bindings(
    .impute_bpca = function(mat, ...) { mat[is.na(mat)] <- stats::runif(sum(is.na(mat))); mat },
    .impute_svd = function(mat, ...) { mat[is.na(mat)] <- stats::runif(sum(is.na(mat))); mat }
  )
  for (fun in list(impute_bpca, impute_svd)) {
    set.seed(555)
    before <- .Random.seed
    first <- fun(x, seed = 72)
    expect_identical(.Random.seed, before)
    set.seed(888)
    second <- fun(x, seed = 72)
    expect_identical(SummarizedExperiment::assay(first),
                     SummarizedExperiment::assay(second))
    different <- fun(x, seed = 73)
    expect_false(identical(SummarizedExperiment::assay(first),
                           SummarizedExperiment::assay(different)))
  }
})

test_that("BPCA and SVD retain positional pca arguments and seeded CV", {
  skip_if_not_installed("pcaMethods")
  x <- seed_impute_exp()[seq_len(15), seq_len(10)]
  for (fun in list(impute_bpca, impute_svd)) {
    set.seed(654)
    before <- .Random.seed
    result <- suppressWarnings(fun(x, NULL, "none", cv = "q2", seed = 42))
    expect_identical(.Random.seed, before)
    set.seed(321)
    repeated <- suppressWarnings(fun(x, scale = "none", cv = "q2", seed = 42))
    expect_identical(SummarizedExperiment::assay(result),
                     SummarizedExperiment::assay(repeated))
    expect_false(anyNA(SummarizedExperiment::assay(result)))
  }
})

test_that("all added imputation seeds are validated and restore absent RNG state", {
  x <- seed_impute_exp()
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", .GlobalEnv) else NULL
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv)
    else if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  })
  for (fun in list(impute_sw_knn, impute_fw_knn, impute_bpca, impute_svd)) {
    for (bad in list(NULL, NA_integer_, -1, 1.5, "123")) {
      expect_error(fun(x, seed = bad))
    }
    if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    quiet_seed_call(fun, x, seed = 12)
    expect_false(exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    expect_identical(RNGkind(), old_kind)
  }
})

test_that("imputation failures restore caller RNG and auto BPCA forwards seed", {
  x <- seed_impute_exp()
  local_mocked_bindings(.impute_bpca = function(mat, ...) {
    stats::runif(2)
    stop("intentional seed failure")
  })
  set.seed(271)
  before <- .Random.seed
  expect_error(impute_bpca(x, seed = 2), "intentional seed failure")
  expect_identical(.Random.seed, before)
  seen <- NULL
  local_mocked_bindings(
    .get_exp_type = function(x) "glycomics",
    impute_bpca = function(x, by = NULL, ..., seed = 123) { seen <<- seed; x }
  )
  # Thirty glycomics samples select BPCA.
  auto_impute(x, group_col = NULL, seed = 987)
  expect_identical(seen, 987)
})
