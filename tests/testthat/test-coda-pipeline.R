coda_pipeline_input <- function(large = FALSE) {
  mat <- rbind(c(8, 8, 8, 8, 8), c(16, 16, 32, 32, 32),
               c(4, 4, 2, 2, 2), c(2, 2, 8, 8, 8), c(32, 32, 16, 16, 16))
  if (large) mat <- rbind(mat[1, ], mat[rep(2:5, length.out = 50), ])
  dimnames(mat) <- list(paste0("V", seq_len(nrow(mat))), paste0("S", 1:5))
  test_glycomic_se(mat, sample_info = tibble::tibble(
    sample = colnames(mat), group = factor(c("A", "A", "B", "B", "B"))))
}

coda_pipeline_stale_input <- function(x) {
  metadata <- .get_container_metadata(x)
  metadata$scale <- "linear"
  metadata$custom <- list(keep = TRUE, label = "preserved")
  metadata$coda_transform <- "alr"
  metadata$coda_reference <- "obsolete-reference"
  metadata$coda_site_transforms <- tibble::tibble(
    protein = "obsolete-protein", protein_site = 999L,
    transform = "alr", reference = "obsolete-reference"
  )
  if (inherits(x, "glyexp_experiment")) {
    x$meta_data <- metadata
  } else {
    S4Vectors::metadata(x) <- metadata
  }
  x
}

test_that("CoDA records public functions and ordinary complete parameters", {
  x <- coda_pipeline_input()
  S4Vectors::metadata(x)$custom <- "keep"
  for (fun in list(transform_clr, transform_alr)) {
    result <- fun(x, by = "group")
    history <- get_pipeline(result)
    expect_length(history, 1L)
    expect_named(history[[1L]], c("function", "parameters"))
    expect_identical(history[[1L]][["function"]], fun)
    expect_identical(history[[1L]]$parameters,
                     list(by = SummarizedExperiment::colData(x)$group, gamma = 0.1,
                          group_scales = NULL, seed = 123))
    expect_null(attr(history[[1L]], "rng_state"))
    expect_null(attr(history[[1L]], "rng_kind"))
    expect_false("site_plan" %in% names(history[[1L]]$parameters))
    expect_length(SummarizedExperiment::assays(result), 1L)
    expect_identical(S4Vectors::metadata(result)$custom, "keep")
    expect_false(any(c("coda_transform", "coda_site_transforms") %in%
                       names(S4Vectors::metadata(result))))
    set.seed(918)
    expect_identical(apply_pipeline(x, history), result)
    restored <- unserialize(serialize(history, NULL))
    expect_identical(apply_pipeline(x, restored), result)
  }
})

test_that("direct and recorded CoDA return identical complete experiments", {
  x <- coda_pipeline_input()
  for (fun in list(transform_clr, transform_alr, auto_coda)) {
    set.seed(11)
    result <- suppressMessages(fun(x, by = "group", gamma = 0.2, seed = 981))
    set.seed(942)
    state <- .Random.seed
    expect_identical(suppressMessages(fun(x, by = "group", gamma = 0.2, seed = 981)),
                     result)
    expect_identical(.Random.seed, state)
    expect_identical(apply_pipeline(x, get_pipeline(result)), result)
    expect_identical(.Random.seed, state)
  }
  alr <- transform_alr(x, by = "group", gamma = 0.2, seed = 981)
  expect_identical(S4Vectors::metadata(alr)$coda_reference, "V1")
  expect_equal(nrow(alr), nrow(x) - 1L)
})

test_that("ALR fallback records ALR and repeats the full reference assessment", {
  original <- coda_pipeline_stale_input(coda_pipeline_input())
  x <- impute_zero(original)
  original_select <- .select_alr_reference
  calls <- 0L
  local_mocked_bindings(.select_alr_reference = function(mat, by_values = NULL) {
    calls <<- calls + 1L
    selected <- original_select(mat, by_values)
    selected$procrustes <- 0
    selected
  })
  expect_warning(result <- transform_alr(x, by = "group", gamma = 0.3, seed = 712),
                 "falling back to CLR")
  expect_identical(calls, 1L)
  metadata <- S4Vectors::metadata(result)
  expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                     names(metadata)))
  keep <- setdiff(names(S4Vectors::metadata(x)),
                  c("pipeline", "coda_transform", "coda_site_transforms", "coda_reference"))
  expect_identical(metadata[keep], S4Vectors::metadata(x)[keep])
  history <- get_pipeline(result)
  expect_length(history, 2L)
  expect_identical(history[[1L]], get_pipeline(x)[[1L]])
  expect_identical(history[[2L]][["function"]], transform_alr)
  expect_identical(history[[2L]]$parameters$seed, 712)
  set.seed(921)
  expect_warning(replay <- apply_pipeline(original, history), "falling back to CLR")
  expect_identical(calls, 2L)
  expect_identical(replay, result)
  # The assessment consumes RNG before fallback; replacing ALR with CLR would
  # reset the stream at the wrong point and change the actual transformed assay.
  clr <- transform_clr(x, by = "group", gamma = 0.3, seed = 712)
  expect_false(identical(SummarizedExperiment::assay(clr),
                         SummarizedExperiment::assay(result)))
})

test_that("ALR fallback without an eligible reference removes stale metadata", {
  original <- coda_pipeline_stale_input(coda_pipeline_input())
  SummarizedExperiment::assay(original)[-1L, 1L] <- 0
  x <- impute_zero(original)
  expect_warning(result <- transform_alr(x, by = "group", gamma = 0.2, seed = 712),
                 "at least two glycans")
  metadata <- S4Vectors::metadata(result)
  expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                     names(metadata)))
  expect_identical(metadata$scale, S4Vectors::metadata(original)$scale)
  expect_identical(metadata$custom, S4Vectors::metadata(original)$custom)
  expect_identical(rownames(result), rownames(original))
  history <- get_pipeline(result)
  expect_length(history, 2L)
  expect_identical(history[[1L]], get_pipeline(x)[[1L]])
  expect_identical(history[[2L]][["function"]], transform_alr)
  expect_warning(replay <- apply_pipeline(original, history), "at least two glycans")
  expect_identical(replay, result)
})

test_that("glycomic auto CoDA records its selected public transformation", {
  for (large in c(FALSE, TRUE)) {
    x <- coda_pipeline_input(large)
    result <- suppressMessages(auto_coda(x, by = "group", gamma = 0.2, seed = 828))
    history <- get_pipeline(result)
    expected <- if (large) transform_alr else transform_clr
    expect_length(history, 1L)
    expect_identical(history[[1L]][["function"]], expected)
    expect_identical(result, expected(x, by = "group", gamma = 0.2, seed = 828))
    expect_identical(apply_pipeline(x, history), result)
  }
})

test_that("glycoproteomic auto records one CLR with the original site grouping", {
  mat <- outer(seq_len(104), c(2, 3, 5, 7, 11))
  dimnames(mat) <- list(paste0("V", seq_len(nrow(mat))), paste0("S", 1:5))
  x <- test_glycoproteomic_se(mat,
    sample_info = tibble::tibble(sample = colnames(mat),
      group = factor(c("A", "A", "B", "B", "B"))),
    var_info = tibble::tibble(variable = rownames(mat),
      protein = rep(c("P2", "P1"), 52),
      protein_site = rep(c(20L, 10L), 52)))
  result <- suppressMessages(auto_coda(x, by = "group", gamma = 0.3, seed = 87))
  expect_identical(result, transform_clr(x, by = "group", gamma = 0.3, seed = 87))
  expect_identical(SummarizedExperiment::rowData(result), SummarizedExperiment::rowData(x))
  expect_identical(SummarizedExperiment::colData(result), SummarizedExperiment::colData(x))
  expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                     names(S4Vectors::metadata(result))))
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(history[[1L]][["function"]], transform_clr)
  expect_identical(history[[1L]]$parameters$seed, 87)
  expect_false("site_plan" %in% names(history[[1L]]$parameters))
  set.seed(928)
  expect_identical(apply_pipeline(x, history), result)
  expect_identical(suppressMessages(auto_coda(x, by = "group", gamma = 0.3, seed = 87)),
                   result)
  expect_error(transform_alr(x), "glycoproteomics|Glycoproteomic|not support")
})

test_that("CoDA appends once and preserves the complete prior history", {
  original <- coda_pipeline_input()
  x <- normalize_total_area(original)
  result <- transform_clr(x, gamma = 0.2, seed = 23)
  expect_length(get_pipeline(result), 2L)
  expect_identical(get_pipeline(result)[[1L]], get_pipeline(x)[[1L]])
  expect_identical(apply_pipeline(original, get_pipeline(result)), result)
})

test_that("CoDA cleans stale metadata and replays complete containers from the original", {
  withr::local_options(lifecycle_verbosity = "quiet")
  withr::local_seed(91)
  glycomic <- coda_pipeline_input()
  glycoproteomic <- test_glycoproteomic_se(
    SummarizedExperiment::assay(glycomic),
    sample_info = .get_sample_info(glycomic),
    var_info = tibble::tibble(variable = rownames(glycomic),
      protein = c("P1", "P2", "P1", "P2", "P1"),
      protein_site = rep(10L, nrow(glycomic))))
  legacy_glycomic <- glyexp::experiment(
    SummarizedExperiment::assay(glycomic),
    sample_info = .get_sample_info(glycomic),
    var_info = .get_var_info(glycomic), exp_type = "glycomics", glycan_type = "N")
  legacy_glycoproteomic <- glyexp::experiment(
    SummarizedExperiment::assay(glycoproteomic),
    sample_info = .get_sample_info(glycoproteomic),
    var_info = .get_var_info(glycoproteomic),
    exp_type = "glycoproteomics", glycan_type = "N")

  for (input in list(glycomic, plain_se(glycomic), legacy_glycomic,
                     glycoproteomic, legacy_glycoproteomic)) {
    original <- coda_pipeline_stale_input(input)
    unchanged <- original
    x <- impute_zero(original)
    input_metadata <- .get_container_metadata(x)
    keep <- setdiff(names(input_metadata),
                    c("pipeline", "coda_transform", "coda_site_transforms", "coda_reference"))
    transforms <- if (.get_exp_type(x) == "glycoproteomics") {
      list(transform_clr, auto_coda)
    } else {
      list(transform_clr, transform_alr)
    }
    for (fun in transforms) {
      result <- suppressMessages(fun(x, by = "group", gamma = 0.2, seed = 981))
      metadata <- .get_container_metadata(result)
      expect_false(any(c("coda_transform", "coda_site_transforms") %in% names(metadata)))
      if (identical(fun, transform_alr)) {
        expect_identical(metadata$coda_reference, "V1")
        expect_identical(metadata$coda_reference,
                         setdiff(rownames(.get_expr_mat(x)), rownames(.get_expr_mat(result))))
      } else {
        expect_false("coda_reference" %in% names(metadata))
      }
      expect_identical(metadata[keep], input_metadata[keep])
      history <- get_pipeline(result)
      expect_length(history, 2L)
      expect_identical(history[[1L]], get_pipeline(x)[[1L]])
      expected_fun <- if (identical(fun, auto_coda)) transform_clr else fun
      expect_identical(history[[2L]][["function"]], expected_fun)
      expect_identical(history[[2L]]$parameters$seed, 981)
      set.seed(942)
      state <- .Random.seed
      expect_identical(apply_pipeline(original, history), result)
      expect_identical(.Random.seed, state)
      expect_identical(original, unchanged)
      expect_identical(.get_container_metadata(x), input_metadata)
    }
  }
})

test_that("CoDA restores absent RNG state on direct execution and replay", {
  x <- coda_pipeline_input()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  for (fun in list(transform_clr, transform_alr, auto_coda)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    result <- suppressMessages(fun(x, by = "group", seed = 128))
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    expect_identical(apply_pipeline(x, get_pipeline(result)), result)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
})

test_that("CoDA errors after RNG consumption restore state without mutating input", {
  x <- coda_pipeline_input()
  unchanged <- x
  local_mocked_bindings(.glycowork_clr_log = function(...) {
    stats::rnorm(9)
    stop("deliberate CoDA failure")
  })
  for (fun in list(transform_clr, transform_alr, auto_coda)) {
    set.seed(552)
    state <- .Random.seed
    expect_error(suppressMessages(fun(x, seed = 91)), "deliberate CoDA failure")
    expect_identical(.Random.seed, state)
    expect_identical(x, unchanged)
    rm(".Random.seed", envir = .GlobalEnv)
    expect_error(suppressMessages(fun(x, seed = 91)), "deliberate CoDA failure")
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  }
  set.seed(552)
  state <- .Random.seed
  recipe <- glyclean_pipeline(transform_clr(seed = 91))
  expect_error(apply_pipeline(x, recipe), "deliberate CoDA failure")
  expect_identical(.Random.seed, state)
  expect_identical(x, unchanged)
})
