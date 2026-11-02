coda_pipeline_input <- function() {
  mat <- outer(c(1, 3, 7, 12, 20, 30), c(2, 4, 5, 8))
  rownames(mat) <- paste0("V", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  test_glycomic_se(mat)
}

test_that("recorded stochastic CLR replays and retains one assay", {
  x <- coda_pipeline_input()
  S4Vectors::metadata(x)$custom <- "keep"
  set.seed(411)
  initial_state <- .Random.seed
  result <- transform_clr(x, gamma = 0.2, seed = 981)
  expect_identical(.Random.seed, initial_state)
  pipeline <- get_pipeline(result)
  expect_length(pipeline, 1L)
  expect_named(pipeline[[1L]], c("function", "parameters"))
  plan <- pipeline[[1L]]$parameters$site_plan
  expect_identical(plan[[1L]][["function"]], .transform_clr_mat)
  expect_true(isNamespace(environment(plan[[1L]][["function"]])))
  set.seed(921)
  state <- .Random.seed
  replay <- apply_pipeline(x, pipeline)
  expect_identical(.Random.seed, state)
  expect_identical(SummarizedExperiment::assay(replay),
                   SummarizedExperiment::assay(result))
  expect_length(SummarizedExperiment::assays(replay), 1L)
  expect_length(get_pipeline(replay), 1L)
  expect_identical(S4Vectors::metadata(replay)$custom, "keep")
})

test_that("ALR fallback preserves randomness consumed by reference selection", {
  x <- coda_pipeline_input()
  # Force a rejected reference after the real selection path consumed RNG.
  original_select <- .select_alr_reference
  local_mocked_bindings(.select_alr_reference = function(mat, by_values = NULL) {
    result <- original_select(mat, by_values)
    result$procrustes <- 0
    result
  })
  expect_warning(result <- transform_alr(x, gamma = 0.3, seed = 712),
                 "falling back to CLR")
  pipeline <- get_pipeline(result)
  expect_identical(pipeline[[1L]]$parameters$site_plan[[1L]][["function"]],
                   .transform_clr_mat)
  replay <- apply_pipeline(x, pipeline)
  expect_identical(SummarizedExperiment::assay(replay),
                   SummarizedExperiment::assay(result))
  expect_identical(S4Vectors::metadata(replay)$coda_transform, "clr")
})

test_that("recorded site-wise mixed CoDA replays concrete decisions", {
  mat <- outer(seq_len(53), c(2, 3, 4, 5))
  rownames(mat) <- paste0("V", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))
  x <- test_glycoproteomic_se(mat,
    var_info = tibble::tibble(variable = rownames(mat),
      protein = c(rep("P1", 51), "P2", "P2"),
      protein_site = rep(10L, 53)))
  local_mocked_bindings(.select_alr_reference = function(mat, by_values = NULL) {
    # Simulate a real selection consuming RNG, then choose a fixed valid ref.
    stats::rnorm(7)
    list(ref_idx = 1L, reference = rownames(mat)[1L],
         procrustes = 1, variance = 0)
  })
  result <- auto_coda(x, gamma = 0.2, seed = 828)
  pipeline <- get_pipeline(result)
  plan <- pipeline[[1L]]$parameters$site_plan
  expect_length(plan, 2L)
  expect_identical(plan[[1L]][["function"]], .apply_alr_reference)
  expect_identical(plan[[2L]][["function"]], .transform_clr_mat)
  expect_identical(S4Vectors::metadata(result)$coda_transform, "mixed")
  replay <- apply_pipeline(x, pipeline)
  expect_identical(SummarizedExperiment::assay(replay),
                   SummarizedExperiment::assay(result))
  expect_identical(SummarizedExperiment::rowData(replay),
                   SummarizedExperiment::rowData(result))
  expect_identical(S4Vectors::metadata(replay)$coda_site_transforms,
                   S4Vectors::metadata(result)$coda_site_transforms)
  # A serialized history has no captured live experiment/trace environment.
  restored <- unserialize(serialize(pipeline, NULL))
  expect_identical(SummarizedExperiment::assay(apply_pipeline(x, restored)),
                   SummarizedExperiment::assay(result))
  expect_error(apply_pipeline(x[rev(seq_len(nrow(x))), ], pipeline),
               "identities and order")
})

test_that("CoDA appends history and removes obsolete ALR metadata on replay", {
  original <- coda_pipeline_input()
  x <- normalize_total_area(original)
  S4Vectors::metadata(x)$coda_reference <- "old-reference"
  result <- transform_clr(x, gamma = 0, seed = 23)
  expect_length(get_pipeline(result), 2L)
  # Replay just the CoDA step against its actual intermediate input.
  last <- structure(list(get_pipeline(result)[[2L]]),
                    class = c("glyclean_pipeline", "list"))
  replay <- apply_pipeline(x, last)
  expect_identical(S4Vectors::metadata(replay), S4Vectors::metadata(result))
  expect_false("coda_reference" %in% names(S4Vectors::metadata(replay)))
  expect_length(get_pipeline(replay), 2L)
})

test_that("CoDA replay restores RNG kind and an absent caller RNG state", {
  x <- coda_pipeline_input()
  pipeline <- get_pipeline(transform_clr(x, gamma = 0.2, seed = 981))
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
  RNGkind("L'Ecuyer-CMRG")
  caller_kind <- RNGkind()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  apply_pipeline(x, pipeline)
  expect_identical(RNGkind(), caller_kind)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("CoDA records uniform step RNG context even before any random draw", {
  x <- coda_pipeline_input()
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
  for (gamma in c(0, 0.2)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    result <- transform_clr(x, gamma = gamma, seed = 128)
    history <- get_pipeline(result)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_type(attr(history[[1L]], "rng_state"), "integer")
    expect_identical(attr(history[[1L]], "rng_kind"), old_kind)
    replay <- apply_pipeline(x, history)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_identical(S4Vectors::metadata(replay), S4Vectors::metadata(result))
    expect_identical(SummarizedExperiment::assay(replay),
                     SummarizedExperiment::assay(result))
  }
})
