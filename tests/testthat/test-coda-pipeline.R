coda_pipeline_input <- function(large = FALSE) {
  mat <- rbind(c(8, 8, 8, 8, 8), c(16, 16, 32, 32, 32),
               c(4, 4, 2, 2, 2), c(2, 2, 8, 8, 8), c(32, 32, 16, 16, 16))
  if (large) mat <- rbind(mat[1, ], mat[rep(2:5, length.out = 50), ])
  dimnames(mat) <- list(paste0("V", seq_len(nrow(mat))), paste0("S", 1:5))
  test_glycomic_se(mat, sample_info = tibble::tibble(
    sample = colnames(mat), group = factor(c("A", "A", "B", "B", "B"))))
}

coda_pipeline_existing_metadata_input <- function(x) {
  metadata <- .get_container_metadata(x)
  metadata$scale <- "raw"
  metadata$custom <- list(keep = TRUE, label = "preserved")
  # These opaque values must not be interpreted, validated, or rewritten.
  metadata$coda_transform <- list(keep = TRUE, values = c(NA_integer_, 7L))
  metadata$coda_reference <- c("obsolete-reference", "another-reference")
  metadata$coda_site_transforms <- tibble::tibble(
    unrelated = "preserved", values = list(c(1L, 3L))
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
    expect_identical(S4Vectors::metadata(result)$scale, "log2")
    expect_identical(S4Vectors::metadata(result)$custom, "keep")
    expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
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
  expect_identical(setdiff(rownames(x), rownames(alr)), "V1")
  expect_equal(nrow(alr), nrow(x) - 1L)
})

test_that("ALR fallback records ALR and repeats the full reference assessment", {
  original_select <- .select_alr_reference
  calls <- 0L
  local_mocked_bindings(.select_alr_reference = function(mat, by_values = NULL) {
    calls <<- calls + 1L
    selected <- original_select(mat, by_values)
    selected$procrustes <- 0
    selected
  })
  input <- coda_pipeline_input()
  for (original in list(input, coda_pipeline_existing_metadata_input(input))) {
    calls <- 0L
    x <- impute_zero(original)
    input_metadata <- S4Vectors::metadata(x)
    expect_warning(result <- transform_alr(x, by = "group", gamma = 0.3, seed = 712),
                   "falling back to CLR")
    expect_identical(calls, 1L)
    metadata <- S4Vectors::metadata(result)
    expect_identical(metadata$scale, "log2")
    expect_identical(metadata[setdiff(names(metadata), c("pipeline", "scale"))],
                     input_metadata[setdiff(names(input_metadata), c("pipeline", "scale"))])
    expect_identical(rownames(result), rownames(x))
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
  }
})

test_that("ALR fallback without an eligible reference preserves existing metadata", {
  input <- coda_pipeline_input()[1L, , drop = FALSE]
  for (original in list(input, coda_pipeline_existing_metadata_input(input))) {
    x <- impute_zero(original)
    input_metadata <- S4Vectors::metadata(x)
    expect_warning(result <- transform_alr(x, by = "group", gamma = 0.2, seed = 712),
                   "at least two glycans")
    metadata <- S4Vectors::metadata(result)
    expect_identical(metadata$scale, "log2")
    expect_identical(metadata[setdiff(names(metadata), c("pipeline", "scale"))],
                     input_metadata[setdiff(names(input_metadata), c("pipeline", "scale"))])
    expect_identical(rownames(result), rownames(original))
    history <- get_pipeline(result)
    expect_length(history, 2L)
    expect_identical(history[[1L]], get_pipeline(x)[[1L]])
    expect_identical(history[[2L]][["function"]], transform_alr)
    expect_warning(replay <- apply_pipeline(original, history), "at least two glycans")
    expect_identical(replay, result)
  }
})

test_that("glycomic auto CoDA records its selected public transformation", {
  for (large in c(FALSE, TRUE)) {
    x <- coda_pipeline_input(large)
    result <- suppressMessages(auto_coda(x, by = "group", gamma = 0.2, seed = 828))
    history <- get_pipeline(result)
    expect_identical(S4Vectors::metadata(result)$scale, "log2")
    expected <- if (large) transform_alr else transform_clr
    expect_length(history, 1L)
    expect_identical(history[[1L]][["function"]], expected)
    expect_identical(result, expected(x, by = "group", gamma = 0.2, seed = 828))
    expect_false(any(c("coda_transform", "coda_site_transforms", "coda_reference") %in%
                       names(S4Vectors::metadata(result))))
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

test_that("CoDA preserves metadata and replays complete containers", {
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

  inputs <- list(glycomic, plain_se(glycomic), legacy_glycomic,
                 glycoproteomic, legacy_glycoproteomic)
  inputs <- c(inputs, lapply(inputs, coda_pipeline_existing_metadata_input))
  for (original in inputs) {
    unchanged <- original
    x <- impute_zero(original)
    input_metadata <- .get_container_metadata(x)
    keep <- setdiff(names(input_metadata), c("pipeline", "scale"))
    transforms <- if (.get_exp_type(x) == "glycoproteomics") {
      list(transform_clr, auto_coda)
    } else {
      list(transform_clr, transform_alr)
    }
    for (fun in transforms) {
      result <- suppressMessages(fun(x, by = "group", gamma = 0.2, seed = 981))
      metadata <- .get_container_metadata(result)
      expect_identical(metadata$scale, "log2")
      expect_identical(metadata[setdiff(names(metadata), c("pipeline", "scale"))], input_metadata[keep])
      if (identical(fun, transform_alr)) {
        expect_identical(setdiff(rownames(.get_expr_mat(x)),
                                 rownames(.get_expr_mat(result))), "V1")
        expect_identical(rownames(.get_expr_mat(result)),
                         rownames(.get_expr_mat(x))[-1L])
      } else {
        expect_identical(rownames(.get_expr_mat(result)), rownames(.get_expr_mat(x)))
      }
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

test_that("deterministic CoDA returns mathematical base-2 log ratios", {
  x <- coda_pipeline_input()
  mat <- SummarizedExperiment::assay(x)
  clr <- transform_clr(x, by = "group", gamma = 0)
  expected_clr <- sweep(log2(mat), 2L, colMeans(log2(mat)), "-")
  expect_equal(SummarizedExperiment::assay(clr), expected_clr)
  expect_equal(unname(colSums(SummarizedExperiment::assay(clr))), rep(0, ncol(x)),
               tolerance = 1e-12)
  expect_identical(S4Vectors::metadata(clr)$scale, "log2")

  alr <- transform_alr(x, by = "group", gamma = 0)
  expect_identical(setdiff(rownames(x), rownames(alr)), "V1")
  expected_alr <- log2(sweep(mat[-1L, , drop = FALSE], 2L, mat[1L, ], "/"))
  expect_equal(SummarizedExperiment::assay(alr), expected_alr)
  expect_identical(S4Vectors::metadata(alr)$scale, "log2")
})

test_that("automatic ALR fallback records log2 scale and replays its assessment", {
  original_select <- .select_alr_reference
  local_mocked_bindings(.select_alr_reference = function(mat, by_values = NULL) {
    selected <- original_select(mat, by_values)
    selected$procrustes <- 0
    selected
  })
  x <- coda_pipeline_existing_metadata_input(coda_pipeline_input(large = TRUE))
  expect_warning(
    result <- suppressMessages(auto_coda(x, by = "group", gamma = 0, seed = 712)),
    "falling back to CLR"
  )
  mat <- SummarizedExperiment::assay(x)
  expect_equal(SummarizedExperiment::assay(result),
               sweep(log2(mat), 2L, colMeans(log2(mat)), "-"))
  expect_identical(S4Vectors::metadata(result)$scale, "log2")
  keep <- setdiff(names(S4Vectors::metadata(x)), c("pipeline", "scale"))
  expect_identical(S4Vectors::metadata(result)[keep], S4Vectors::metadata(x)[keep])
  history <- get_pipeline(result)
  expect_identical(history[[1L]][["function"]], transform_alr)
  expect_warning(replay <- apply_pipeline(x, history), "falling back to CLR")
  expect_identical(replay, result)
})

test_that("complete manual CoDA recipes and recorded histories retain log2 scale", {
  original <- coda_pipeline_existing_metadata_input(coda_pipeline_input())
  recipes <- list(
    glyclean_pipeline(impute_zero(), transform_clr(by = "group", gamma = 0.2, seed = 981)),
    glyclean_pipeline(impute_zero(), transform_alr(by = "group", gamma = 0.2, seed = 981)),
    glyclean_pipeline(impute_zero(), auto_coda(by = "group", gamma = 0.2, seed = 981))
  )
  transforms <- list(transform_clr, transform_alr, auto_coda)
  for (i in seq_along(recipes)) {
    manual <- suppressMessages(apply_pipeline(original, recipes[[i]]))
    direct <- suppressMessages(transforms[[i]](
      impute_zero(original), by = "group", gamma = 0.2, seed = 981
    ))
    expect_identical(manual, direct)
    expect_identical(S4Vectors::metadata(manual)$scale, "log2")
    history <- get_pipeline(manual)
    expect_length(history, 2L)
    expect_identical(history[[1L]][["function"]], impute_zero)
    expected_fun <- if (i == 2L) transform_alr else transform_clr
    expect_identical(history[[2L]][["function"]], expected_fun)
    expect_identical(apply_pipeline(original, history), manual)
    expect_identical(apply_pipeline(original, unserialize(serialize(history, NULL))), manual)
    expect_identical(S4Vectors::metadata(original)$scale, "raw")
  }
})

test_that("all public CoDA branches reject invalid abundances without mutation", {
  withr::local_options(lifecycle_verbosity = "quiet")
  glycomic <- coda_pipeline_input()
  glycoproteomic <- test_glycoproteomic_se(
    SummarizedExperiment::assay(glycomic),
    sample_info = .get_sample_info(glycomic),
    var_info = tibble::tibble(variable = rownames(glycomic),
      protein = rep("P1", nrow(glycomic)), protein_site = rep(10L, nrow(glycomic))))
  legacy <- glyexp::experiment(
    .get_expr_mat(glycomic), sample_info = .get_sample_info(glycomic),
    var_info = .get_var_info(glycomic), exp_type = "glycomics", glycan_type = "N")
  inputs <- list(glycomic, coda_pipeline_input(large = TRUE), plain_se(glycomic),
                 glycoproteomic, legacy)
  invalid <- list(zero = 0, all_zero = 0, missing = NA_real_, negative = -1,
                  infinite = Inf)
  patterns <- c(zero = "Zero values", all_zero = "Zero values", missing = "[Mm]issing",
                negative = "[Nn]egative", infinite = "[Ff]inite")
  withr::local_seed(918)
  for (original in inputs) {
    transforms <- if (.get_exp_type(original) == "glycoproteomics") {
      list(transform_clr, auto_coda)
    } else {
      list(transform_clr, transform_alr, auto_coda)
    }
    for (kind in names(invalid)) {
      mat <- .get_expr_mat(original)
      if (kind == "all_zero") mat[,] <- 0 else mat[1L, 1L] <- invalid[[kind]]
      x <- original
      if (inherits(x, "glyexp_experiment")) {
        x$expr_mat <- mat
      } else {
        SummarizedExperiment::assay(x) <- mat
      }
      unchanged <- x
      for (fun in transforms) {
        state <- .Random.seed
        expect_error(suppressMessages(fun(x, by = "group", seed = 81)), patterns[[kind]])
        if (kind %in% c("zero", "all_zero")) {
          expect_error(suppressMessages(fun(x, by = "group", seed = 81)), "zero_to_na")
          expect_error(suppressMessages(fun(x, by = "group", seed = 81)), "imput")
        }
        expect_identical(.Random.seed, state)
        expect_identical(x, unchanged)
        expect_length(get_pipeline(x), 0L)
      }
    }
  }
})

test_that("zero conversion and positive imputation replay before log-ratio CoDA", {
  original <- coda_pipeline_existing_metadata_input(coda_pipeline_input())
  SummarizedExperiment::assay(original)[2L, 1L] <- 0
  recipes <- list(
    glyclean_pipeline(zero_to_na(), impute_half_sample_min(),
                      transform_clr(by = "group", gamma = 0, seed = 981)),
    glyclean_pipeline(zero_to_na(), impute_half_sample_min(),
                      transform_alr(by = "group", gamma = 0, seed = 981)),
    glyclean_pipeline(zero_to_na(), impute_half_sample_min(),
                      auto_coda(by = "group", gamma = 0, seed = 981))
  )
  transforms <- list(transform_clr, transform_alr, auto_coda)
  for (i in seq_along(recipes)) {
    converted <- zero_to_na(original)
    expect_true(is.na(SummarizedExperiment::assay(converted)[2L, 1L]))
    imputed <- impute_half_sample_min(converted)
    expect_true(all(SummarizedExperiment::assay(imputed) > 0))
    direct <- suppressMessages(transforms[[i]](imputed, by = "group", gamma = 0, seed = 981))
    manual <- suppressMessages(apply_pipeline(original, recipes[[i]]))
    expect_identical(manual, direct)
    expect_identical(S4Vectors::metadata(manual)$scale, "log2")
    expect_true(all(is.finite(SummarizedExperiment::assay(manual))))
    history <- get_pipeline(manual)
    expect_length(history, 3L)
    expect_identical(history[[1L]][["function"]], zero_to_na)
    expect_identical(history[[2L]][["function"]], impute_half_sample_min)
    expect_identical(apply_pipeline(original, history), manual)
    expect_identical(apply_pipeline(original, unserialize(serialize(history, NULL))), manual)
    expect_identical(SummarizedExperiment::assay(original)[2L, 1L], 0)
    expect_identical(S4Vectors::metadata(original)$scale, "raw")
  }
})
