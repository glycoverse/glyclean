# Remove the field after construction: current glyexp constructors fill it in.
legacy_scale_set <- function(x, metadata) {
  if (inherits(x, "glyexp_experiment")) {
    x$meta_data <- metadata
  } else {
    S4Vectors::metadata(x) <- metadata
  }
  x
}

legacy_scale_absent <- function(x) {
  metadata <- .get_container_metadata(x)
  metadata$scale <- NULL
  metadata$scale_hint <- "log2"
  metadata$custom <- list(retain = TRUE)
  legacy_scale_set(x, metadata)
}

legacy_scale_fixture <- function(proteomic = FALSE) {
  x <- if (proteomic) {
    simple_glycoproteomic_exp(60, 12)
  } else {
    simple_glycomic_se(60, 12)
  }
  SummarizedExperiment::assay(x) <- withr::with_seed(
    240,
    matrix(exp(rnorm(720, 5, 0.4)), 60, 12, dimnames = dimnames(x))
  )
  SummarizedExperiment::colData(x)$group <- factor(rep(c("A", "B"), 6))
  SummarizedExperiment::colData(x)$batch <- factor(rep(c("I", "II"), each = 6))
  SummarizedExperiment::assayNames(x) <- "signal"
  legacy_scale_absent(x)
}

legacy_scale_expect_processing <- function(
  x,
  fun,
  args = list(),
  replay = TRUE
) {
  before <- serialize(x, NULL)
  metadata <- .get_container_metadata(x)
  expect_false("scale" %in% names(metadata))
  metadata$scale <- "raw"
  raw <- legacy_scale_set(x, metadata)
  result <- suppressMessages(do.call(fun, c(list(x), args)))
  expected <- suppressMessages(do.call(fun, c(list(raw), args)))
  expect_identical(class(result), class(x))
  expect_equal(.get_expr_mat(result), .get_expr_mat(expected))
  expect_identical(.get_var_info(result), .get_var_info(expected))
  expect_identical(.get_sample_info(result), .get_sample_info(expected))
  expect_identical(.get_container_metadata(result)$custom, list(retain = TRUE))
  expect_identical(get_pipeline(result), get_pipeline(expected))
  if (methods::is(result, "SummarizedExperiment")) {
    expect_true(methods::validObject(result))
    expect_identical(SummarizedExperiment::assayNames(result), "signal")
  }
  if (
    methods::is(result, "GlycomicSE") || methods::is(result, "GlycoproteomicSE")
  ) {
    expect_identical(.get_scale(result), .get_scale(expected))
    expect_true("scale" %in% names(.get_container_metadata(result)))
  }
  if (replay) {
    replayed <- suppressMessages(apply_pipeline(x, get_pipeline(result)))
    expect_equal(replayed, result)
    if (methods::is(replayed, "SummarizedExperiment")) {
      expect_true(methods::validObject(replayed))
    }
  }
  expect_identical(serialize(x, NULL), before)
  invisible(result)
}

for (method in c(
  "normalize_median",
  "normalize_median_abs",
  "normalize_total_area",
  "normalize_quantile",
  "normalize_loessf",
  "normalize_loesscyc",
  "normalize_vsn",
  "normalize_median_quotient",
  "normalize_rlr",
  "normalize_rlrma",
  "normalize_rlrmacyc",
  "transform_log",
  "transform_clr",
  "transform_alr",
  "zero_to_na",
  "remove_rare",
  "remove_low_var",
  "remove_low_cv",
  "remove_constant",
  "remove_low_expr"
)) {
  local({
    method <- method
    test_that(paste(method, "accepts absent scale in both glyco subclasses"), {
      if (method == "normalize_vsn") {
        skip_if_not_installed("vsn")
      }
      if (grepl("rlr", method)) {
        skip_if_not_installed("MASS")
      }
      for (proteomic in c(FALSE, TRUE)) {
        if (method == "transform_alr" && proteomic) {
          next
        }
        x <- legacy_scale_fixture(proteomic)
        if (method == "zero_to_na") {
          SummarizedExperiment::assay(x)[1, 1] <- 0
        }
        args <- if (method %in% c("transform_clr", "transform_alr")) {
          list(gamma = 0)
        } else {
          list()
        }
        legacy_scale_expect_processing(x, get(method), args)
      }
    })
  })
}

for (method in c(
  "impute_zero",
  "impute_sample_min",
  "impute_half_sample_min",
  "impute_sw_knn",
  "impute_fw_knn",
  "impute_bpca",
  "impute_ppca",
  "impute_svd",
  "impute_min_prob",
  "impute_miss_forest"
)) {
  local({
    method <- method
    test_that(paste(method, "imputes legacy glyco objects reproducibly"), {
      if (grepl("knn", method)) {
        skip_if_not_installed("impute")
      }
      if (method %in% c("impute_bpca", "impute_ppca", "impute_svd")) {
        skip_if_not_installed("pcaMethods")
      }
      if (method == "impute_miss_forest") {
        skip_if_not_installed("missForest")
      }
      for (proteomic in c(FALSE, TRUE)) {
        x <- legacy_scale_fixture(proteomic)
        SummarizedExperiment::assay(x)[c(1, 75, 149, 222)] <- NA_real_
        args <- if (method == "impute_miss_forest") {
          list(ntree = 10, maxiter = 2)
        } else {
          list()
        }
        result <- legacy_scale_expect_processing(x, get(method), args)
        expect_false(anyNA(.get_expr_mat(result)))
      }
    })
  })
}

for (method in c(
  "auto_aggregate",
  "auto_clean",
  "auto_coda",
  "auto_impute",
  "auto_normalize",
  "auto_remove"
)) {
  local({
    method <- method
    test_that(paste(method, "preserves valid legacy glyco outputs"), {
      withr::local_options(lifecycle_verbosity = "quiet")
      for (proteomic in c(FALSE, TRUE)) {
        x <- legacy_scale_fixture(proteomic)
        args <- switch(
          method,
          auto_aggregate = list(standardize_variable = FALSE),
          auto_clean = list(standardize_variable = FALSE),
          auto_coda = list(gamma = 0),
          list()
        )
        legacy_scale_expect_processing(x, get(method), args)
      }
    })
  })
}

test_that("specialized annotations and protein adjustment accept missing scale", {
  x <- legacy_scale_fixture(TRUE)
  proteins <- as.character(SummarizedExperiment::rowData(x)$protein)
  pro <- matrix(
    2,
    length(proteins),
    ncol(x),
    dimnames = list(proteins, colnames(x))
  )
  fasta <- stats::setNames(rep(strrep("A", 100), length(proteins)), proteins)
  legacy_scale_expect_processing(x, adjust_protein, list(pro_expr_mat = pro))
  legacy_scale_expect_processing(x, add_site_seq, list(fasta = fasta, n_aa = 2))
  for (input in list(x, legacy_scale_fixture())) {
    legacy_scale_expect_processing(
      input,
      aggregate,
      list(standardize_variable = FALSE)
    )
  }
})

test_that("batch correction and every automatic skip path return valid objects", {
  skip_if_not_installed("sva")
  for (proteomic in c(FALSE, TRUE)) {
    x <- legacy_scale_fixture(proteomic)
    legacy_scale_expect_processing(
      x,
      correct_batch_effect,
      list(group = "group")
    )
    legacy_scale_expect_processing(
      x,
      auto_correct_batch_effect,
      list(prop_threshold = 0)
    )
    for (args in list(
      list(batch_col = NULL),
      list(batch_col = "absent"),
      list(prop_threshold = 1)
    )) {
      legacy_scale_expect_processing(x, auto_correct_batch_effect, args)
    }
    SummarizedExperiment::assay(x)[, -1] <- NA_real_
    legacy_scale_expect_processing(x, auto_correct_batch_effect)
  }
})

test_that("empty pipelines repair only glyco subclasses and preserve history", {
  for (x in list(
    legacy_scale_fixture(),
    legacy_scale_fixture(TRUE),
    legacy_scale_absent(plain_se(legacy_scale_fixture())),
    legacy_scale_absent(legacy_exp(6, 6))
  )) {
    before <- serialize(x, NULL)
    expect_length(get_pipeline(x), 0L)
    result <- apply_pipeline(x, glyclean_pipeline())
    expect_identical(class(result), class(x))
    expect_identical(.get_expr_mat(result), .get_expr_mat(x))
    if (methods::is(result, "SummarizedExperiment")) {
      expect_true(methods::validObject(result))
    }
    is_glyco <- methods::is(x, "GlycomicSE") ||
      methods::is(x, "GlycoproteomicSE")
    expect_identical(
      "scale" %in% names(.get_container_metadata(result)),
      is_glyco
    )
    expect_identical(serialize(x, NULL), before)
    for (fun in list(
      impute_zero,
      remove_constant,
      zero_to_na,
      normalize_median
    )) {
      output <- suppressMessages(fun(x))
      expect_identical(
        "scale" %in% names(.get_container_metadata(output)),
        is_glyco
      )
      expect_identical(class(output), class(x))
      if (methods::is(output, "SummarizedExperiment")) {
        expect_true(methods::validObject(output))
      }
    }
  }
})

test_that("filter subsets and auto-remove presets keep repaired metadata", {
  for (proteomic in c(FALSE, TRUE)) {
    for (preset in c("simple", "discovery", "biomarker")) {
      x <- legacy_scale_fixture(proteomic)
      SummarizedExperiment::assay(x)[1, ] <- NA_real_
      result <- legacy_scale_expect_processing(
        x,
        auto_remove,
        list(preset = preset)
      )
      expect_false(rownames(x)[1] %in% rownames(result))
      expect_identical(.get_scale(result), "raw")
    }
  }
})

test_that("explicit invalid scales never pass newly validated no-op paths", {
  for (input in list(
    legacy_scale_fixture(),
    legacy_scale_fixture(TRUE),
    plain_se(legacy_scale_fixture()),
    legacy_exp(6, 6)
  )) {
    for (value in list(
      NULL,
      character(),
      NA_character_,
      "",
      "invalid",
      2,
      TRUE,
      c("raw", "log2"),
      list("raw")
    )) {
      metadata <- .get_container_metadata(input)
      metadata["scale"] <- list(value)
      x <- legacy_scale_set(input, metadata)
      before <- serialize(x, NULL)
      for (fun in list(
        \(x) apply_pipeline(x, glyclean_pipeline()),
        \(x) auto_correct_batch_effect(x, batch_col = NULL),
        \(x) auto_remove(x, preset = "simple"),
        zero_to_na,
        impute_zero,
        remove_constant
      )) {
        expect_error(suppressMessages(fun(x)), "scale|choices|element")
      }
      expect_identical(serialize(x, NULL), before)
    }
  }
})

test_that("known scales survive no-op processing unchanged", {
  for (proteomic in c(FALSE, TRUE)) {
    for (scale in c("raw", "log", "log2", "log10", "logx")) {
      x <- legacy_scale_fixture(proteomic)
      S4Vectors::metadata(x)$scale <- scale
      for (fun in list(
        \(x) apply_pipeline(x, glyclean_pipeline()),
        \(x) auto_correct_batch_effect(x, batch_col = NULL),
        remove_constant
      )) {
        result <- suppressMessages(fun(x))
        expect_identical(.get_scale(result), scale)
        expect_identical(.get_expr_mat(result), .get_expr_mat(x))
        expect_true(methods::validObject(result))
      }
    }
  }
})

for (method in c(
  "plot_missing_bar",
  "plot_missing_heatmap",
  "plot_tic_bar",
  "plot_rank_abundance",
  "plot_int_boxplot",
  "plot_rle",
  "plot_cv_dent",
  "plot_batch_pca",
  "plot_rep_scatter",
  "detect_batch_effect"
)) {
  local({
    method <- method
    test_that(paste(method, "reads legacy scale without modifying its input"), {
      if (method == "plot_missing_heatmap") {
        skip_if_not_installed("pheatmap")
        skip_if_not_installed("ggplotify")
      }
      if (method == "plot_batch_pca") {
        skip_if_not_installed("factoextra")
      }
      if (method == "plot_rep_scatter") {
        skip_if_not_installed("patchwork")
      }
      for (proteomic in c(FALSE, TRUE)) {
        x <- legacy_scale_fixture(proteomic)
        before <- serialize(x, NULL)
        raw <- x
        S4Vectors::metadata(raw)$scale <- "raw"
        args <- if (method == "plot_rep_scatter") {
          list(rep_col = "group", n_pairs = 1)
        } else {
          list()
        }
        actual <- suppressMessages(do.call(get(method), c(list(x), args)))
        expected <- suppressMessages(do.call(get(method), c(list(raw), args)))
        if (method == "detect_batch_effect") {
          expect_equal(actual, expected)
        } else {
          expect_s3_class(actual, "ggplot")
          if (method != "plot_missing_heatmap") {
            expect_equal(
              ggplot2::ggplot_build(actual)$data,
              ggplot2::ggplot_build(expected)$data
            )
          }
        }
        expect_identical(serialize(x, NULL), before)
        expect_false("scale" %in% names(.get_container_metadata(x)))
      }
    })
  })
}

test_that("batch correction warning exits still repair missing glyco scales", {
  skip_if_not_installed("sva")
  for (proteomic in c(FALSE, TRUE)) {
    for (case in c("single", "confounded", "small")) {
      x <- legacy_scale_fixture(proteomic)
      args <- list()
      warning <- switch(
        case,
        single = "Less than 2 batches",
        confounded = "highly confounded",
        small = "fewer than 2 samples"
      )
      if (case == "single") {
        SummarizedExperiment::colData(x)$batch <- rep("I", ncol(x))
      } else if (case == "confounded") {
        args$group <- "batch"
      } else {
        SummarizedExperiment::colData(x)$batch <- c("I", rep("II", ncol(x) - 1))
      }
      before <- serialize(x, NULL)
      raw <- x
      S4Vectors::metadata(raw)$scale <- "raw"
      expect_warning(
        result <- suppressMessages(do.call(
          correct_batch_effect,
          c(list(x), args)
        )),
        warning
      )
      expect_warning(
        expected <- suppressMessages(do.call(
          correct_batch_effect,
          c(list(raw), args)
        )),
        warning
      )
      expect_equal(result, expected)
      expect_true(methods::validObject(result))
      expect_equal(.get_expr_mat(result), .get_expr_mat(x))
      expect_identical(.get_scale(result), "raw")
      expect_warning(
        replayed <- suppressMessages(apply_pipeline(x, get_pipeline(result))),
        warning
      )
      expect_equal(replayed, result)
      expect_true(methods::validObject(replayed))
      expect_identical(serialize(x, NULL), before)
    }
  }
})

test_that("existing history is retained when repairing a legacy processing result", {
  for (proteomic in c(FALSE, TRUE)) {
    original <- legacy_scale_fixture(proteomic)
    x <- legacy_scale_absent(normalize_total_area(original))
    previous <- get_pipeline(x)
    before <- serialize(x, NULL)
    result <- impute_sample_min(x)
    expect_length(get_pipeline(result), length(previous) + 1L)
    expect_identical(
      get_pipeline(result)[seq_along(previous)],
      unclass(previous)
    )
    expect_true(methods::validObject(result))
    expect_identical(.get_scale(result), "raw")
    replayed <- apply_pipeline(original, get_pipeline(result))
    expect_true(methods::validObject(replayed))
    expect_identical(.get_expr_mat(replayed), .get_expr_mat(result))
    expect_identical(
      S4Vectors::metadata(replayed)[names(S4Vectors::metadata(result))],
      S4Vectors::metadata(result)
    )
    expect_identical(serialize(x, NULL), before)
  }
})

test_that("failed batch backend returns a valid repaired legacy container", {
  skip_if_not_installed("sva")
  local_mocked_bindings(
    ComBat = function(...) stop("backend unavailable"),
    .package = "sva"
  )
  for (proteomic in c(FALSE, TRUE)) {
    x <- legacy_scale_fixture(proteomic)
    before <- serialize(x, NULL)
    expect_warning(
      result <- suppressMessages(correct_batch_effect(x)),
      "ComBat failed"
    )
    expect_true(methods::validObject(result))
    expect_identical(class(result), class(x))
    expect_identical(.get_expr_mat(result), .get_expr_mat(x))
    expect_identical(.get_var_info(result), .get_var_info(x))
    expect_identical(.get_sample_info(result), .get_sample_info(x))
    expect_identical(.get_scale(result), "raw")
    expect_length(get_pipeline(result), 1L)
    expect_identical(serialize(x, NULL), before)
  }
})
