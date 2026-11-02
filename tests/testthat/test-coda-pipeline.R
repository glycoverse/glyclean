coda_pipeline_input <- function(large = FALSE) {
  mat <- rbind(c(8, 8, 8, 8, 8), c(16, 16, 32, 32, 32),
               c(4, 4, 2, 2, 2), c(2, 2, 8, 8, 8), c(32, 32, 16, 16, 16))
  if (large) mat <- rbind(mat[1, ], mat[rep(2:5, length.out = 50), ])
  dimnames(mat) <- list(paste0("V", seq_len(nrow(mat))), paste0("S", 1:5))
  test_glycomic_se(mat, sample_info = tibble::tibble(
    sample = colnames(mat), group = factor(c("A", "A", "B", "B", "B"))))
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
  expect_identical(S4Vectors::metadata(alr)$coda_transform, "alr")
  expect_equal(nrow(alr), nrow(x) - 1L)
})

test_that("ALR fallback records ALR and repeats the full reference assessment", {
  x <- coda_pipeline_input()
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
  expect_identical(S4Vectors::metadata(result)$coda_transform, "clr")
  history <- get_pipeline(result)
  expect_identical(history[[1L]][["function"]], transform_alr)
  expect_identical(history[[1L]]$parameters$seed, 712)
  set.seed(921)
  expect_warning(replay <- apply_pipeline(x, history), "falling back to CLR")
  expect_identical(calls, 2L)
  expect_identical(replay, result)
  # The assessment consumes RNG before fallback; replacing ALR with CLR would
  # reset the stream at the wrong point and change the actual transformed assay.
  clr <- transform_clr(x, by = "group", gamma = 0.3, seed = 712)
  expect_false(identical(SummarizedExperiment::assay(clr),
                         SummarizedExperiment::assay(result)))
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
  expect_identical(S4Vectors::metadata(result)$coda_site_transforms$transform,
                   c("clr", "clr"))
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
