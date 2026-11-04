# Input fixtures deliberately store equivalent abundances in each known scale.
scale_input <- function(x, scale) {
  metadata <- .get_container_metadata(x)
  metadata$scale <- scale
  .rebuild_container(
    x,
    expr_mat = .from_work_scale(.get_expr_mat(x), scale),
    metadata = metadata
  )
}

scale_test_matrix <- function(n = 60, m = 8, missing = FALSE) {
  mat <- withr::with_seed(91, matrix(exp(rnorm(n * m, 3, 1)), n, m))
  if (missing) {
    mat[cbind(c(3, 5, 8, 12), c(1, 3, 5, 7))] <- NA_real_
  }
  test_glycomic_se(
    mat,
    sample_info = tibble::tibble(
      sample = paste0("S", seq_len(m)),
      group = rep(c("A", "B"), length.out = m),
      batch = rep(c("X", "X", "Y", "Y"), length.out = m),
      replicate = rep(c("R1", "R2"), each = m / 2)
    )
  )
}

known_scales <- c("raw", "log", "log2", "log10")

test_that("ordinary normalization scales give the same abundances", {
  x <- scale_test_matrix()
  methods <- list(
    normalize_total_area,
    normalize_median,
    normalize_median_abs,
    normalize_median_quotient,
    normalize_quantile,
    normalize_loessf,
    normalize_loesscyc,
    normalize_rlr,
    normalize_rlrma,
    normalize_rlrmacyc
  )
  for (fun in methods) {
    expected <- .get_expr_mat(fun(x))
    for (scale in known_scales) {
      input <- scale_input(x, scale)
      result <- fun(input)
      expect_identical(.get_scale(result), scale)
      expect_equal(
        .to_work_scale(.get_expr_mat(result), scale),
        expected,
        tolerance = 1e-7
      )
      expect_identical(apply_pipeline(input, get_pipeline(result)), result)
    }
  }
})

test_that("grouped normalization retains singleton matrix dimensions", {
  x <- scale_input(scale_test_matrix(1, 8), "log10")
  for (groups in list(rep(1, 8), rep(1:4, each = 2), seq_len(8))) {
    out <- normalize_quantile(x, by = factor(groups))
    expect_equal(dim(.get_expr_mat(out)), c(1L, 8L))
    expect_identical(apply_pipeline(x, get_pipeline(out)), out)
  }
  x <- scale_test_matrix(10, 8)
  for (scale in known_scales) {
    out <- normalize_median_quotient(scale_input(x, scale), by = "group")
    ref <- normalize_median_quotient(x, by = "group")
    expect_equal(.to_work_scale(.get_expr_mat(out), scale), .get_expr_mat(ref))
  }
})

test_that("even medians and raw quantiles retain arithmetic raw semantics", {
  x <- test_glycomic_se(matrix(c(1, 9, 4, 100), 2))
  expected <- normalize_median(x)
  input <- scale_input(x, "log2")
  out <- normalize_median(input)
  expect_equal(2^.get_expr_mat(out), .get_expr_mat(expected))
  expect_equal(
    2^.get_expr_mat(normalize_quantile(input)),
    .get_expr_mat(normalize_quantile(x))
  )
  expect_equal(
    apply(.get_expr_mat(out), 2, .log2_median),
    rep(mean(log2(c(5, 52))), 2),
    ignore_attr = TRUE
  )
})

test_that("imputations work in their declared domains and retain RNG", {
  x <- scale_test_matrix(missing = TRUE)
  methods <- list(
    impute_sample_min,
    impute_half_sample_min,
    impute_min_prob,
    impute_sw_knn,
    impute_fw_knn,
    impute_bpca,
    impute_ppca,
    impute_svd,
    function(x) impute_miss_forest(x, ntree = 10, maxiter = 2)
  )
  for (fun in methods) {
    expected <- suppressWarnings(fun(x))
    for (scale in known_scales) {
      input <- scale_input(x, scale)
      withr::local_seed(12)
      state <- .Random.seed
      result <- suppressWarnings(fun(input))
      expect_identical(.Random.seed, state)
      expect_identical(.get_scale(result), scale)
      expect_equal(
        .to_work_scale(.get_expr_mat(result), scale),
        .get_expr_mat(expected),
        tolerance = 1e-6
      )
      expect_identical(
        suppressWarnings(apply_pipeline(input, get_pipeline(result))),
        result
      )
    }
  }
})

test_that("sample minima handle missing and singleton columns without Inf", {
  x <- test_glycomic_se(matrix(c(NA, NA, 2, NA), 2))
  for (scale in known_scales) {
    input <- scale_input(x, scale)
    for (fun in list(impute_sample_min, impute_half_sample_min)) {
      result <- fun(input)
      expect_true(all(is.na(.get_expr_mat(result)[, 1])))
      expect_true(all(is.finite(.get_expr_mat(result)[, 2])))
      expect_equal(dim(.get_expr_mat(result)), c(2L, 2L))
    }
  }
  x <- test_glycomic_se(matrix(c(2, NA), 1))
  expect_equal(dim(.get_expr_mat(impute_sample_min(x))), c(1L, 2L))
})

test_that("filtering uses raw units but preserves original assay exactly", {
  x <- scale_test_matrix(missing = TRUE)
  mat <- .get_expr_mat(x)
  mat[1, ] <- 10
  SummarizedExperiment::assay(x) <- mat
  methods <- list(
    function(x) remove_rare(x, prop = 0.1),
    remove_constant,
    function(x) remove_low_cv(x, cv_cutoff = 0.5, by = "group"),
    function(x) remove_low_var(x, var_cutoff = 50, by = "group"),
    function(x) remove_low_expr(x, percentile = 0.2, by = "group")
  )
  for (fun in methods) {
    rows <- rownames(suppressMessages(fun(x)))
    for (scale in known_scales) {
      input <- scale_input(x, scale)
      result <- suppressMessages(fun(input))
      expect_identical(rownames(result), rows)
      expect_identical(
        .get_expr_mat(result),
        .get_expr_mat(input)[rows, , drop = FALSE]
      )
      expect_identical(.get_scale(result), scale)
      expect_identical(
        suppressMessages(apply_pipeline(input, get_pipeline(result))),
        result
      )
    }
  }
})

test_that("zero contracts distinguish abundance zero from coordinate zero", {
  x <- test_glycomic_se(matrix(c(0, 1, NA, 8), 2))
  for (scale in known_scales) {
    input <- scale_input(x, scale)
    result <- zero_to_na(input)
    expect_true(is.na(.get_expr_mat(result)[1, 1]))
    expect_identical(.get_expr_mat(result)[2, 1], .get_expr_mat(input)[2, 1])
    expect_identical(.get_scale(result), scale)
    expect_identical(apply_pipeline(input, get_pipeline(result)), result)
    if (scale != "raw") expect_error(impute_zero(input), "only supports.*raw")
  }
  expect_identical(.get_expr_mat(impute_zero(x))[1, 2], 0)
  x <- scale_input(x, "log2")
  mat <- .get_expr_mat(x)
  mat[1, 2] <- Inf
  mat[2, 2] <- NaN
  SummarizedExperiment::assay(x) <- mat
  out <- .get_expr_mat(zero_to_na(x))
  expect_identical(out[1, 2], Inf)
  expect_true(is.nan(out[2, 2]))
  expect_identical(out[2, 1], 0)
})

test_that("aggregation sum and callbacks consume raw values", {
  x <- aggregation_glycomic_se()
  mat <- .get_expr_mat(x)
  mat[1:3, 1] <- NA_real_
  SummarizedExperiment::assay(x) <- mat
  for (fun in list(sum, mean, function(values) sum(values) + 7)) {
    ref <- aggregate(x, to_level = "g", f = fun, standardize_variable = FALSE)
    for (scale in known_scales) {
      input <- scale_input(x, scale)
      out <- aggregate(
        input,
        to_level = "g",
        f = fun,
        standardize_variable = FALSE
      )
      expect_equal(
        .to_work_scale(.get_expr_mat(out), scale),
        .get_expr_mat(ref)
      )
      expect_identical(.get_scale(out), scale)
      expect_identical(apply_pipeline(input, get_pipeline(out)), out)
    }
  }
  out <- aggregate(
    scale_input(x, "log2"),
    to_level = "g",
    standardize_variable = FALSE
  )
  expect_identical(.get_expr_mat(out)[1, 1], -Inf)
})

test_that("logx support policies accept legal negative and zero coordinates", {
  x <- scale_input(scale_test_matrix(), "log2")
  mat <- .get_expr_mat(x) - 5
  mat[1, 1] <- 0
  meta <- .get_container_metadata(x)
  meta$scale <- "logx"
  x <- .rebuild_container(x, expr_mat = mat, metadata = meta)
  rejected <- list(
    normalize_total_area,
    normalize_median,
    normalize_median_abs,
    normalize_median_quotient,
    normalize_vsn,
    impute_zero,
    impute_half_sample_min,
    impute_min_prob,
    zero_to_na,
    remove_low_cv,
    remove_low_expr,
    remove_low_var,
    transform_clr,
    transform_alr,
    auto_coda,
    auto_normalize,
    auto_clean,
    function(x) aggregate(x, standardize_variable = FALSE),
    plot_tic_bar,
    plot_cv_dent,
    plot_rank_abundance
  )
  for (fun in rejected) {
    expect_error(suppressMessages(fun(x)), "logx|only supports.*raw")
  }
  allowed <- list(
    normalize_quantile,
    normalize_loessf,
    normalize_loesscyc,
    normalize_rlr,
    normalize_rlrma,
    normalize_rlrmacyc,
    impute_sample_min,
    remove_rare,
    remove_constant,
    auto_remove
  )
  for (fun in allowed) {
    out <- suppressMessages(fun(x))
    expect_identical(.get_scale(out), "logx")
    expect_true(any(.get_expr_mat(out) < 0))
    expect_identical(
      suppressMessages(apply_pipeline(x, get_pipeline(out))),
      out
    )
  }
  expect_identical(.get_expr_mat(impute_sample_min(x)), mat)
  expect_identical(.get_expr_mat(remove_constant(x)), mat)
  expect_error(suppressMessages(auto_impute(x)), "logx")
  for (fun in list(
    impute_sw_knn,
    impute_fw_knn,
    impute_bpca,
    impute_ppca,
    impute_svd,
    function(x) impute_miss_forest(x, ntree = 10, maxiter = 2)
  )) {
    input <- x
    SummarizedExperiment::assay(input)[2, 1] <- NA_real_
    out <- suppressWarnings(fun(input))
    expect_identical(.get_scale(out), "logx")
    expect_true(any(.get_expr_mat(out) < 0))
    expect_identical(
      suppressWarnings(apply_pipeline(input, get_pipeline(out))),
      out
    )
  }
})

test_that("CoDA accepts known log scales and outputs logx", {
  x <- test_glycomic_se(rbind(
    c(8, 8, 8, 8, 8),
    c(16, 16, 32, 32, 32),
    c(4, 4, 2, 2, 2),
    c(2, 2, 8, 8, 8),
    c(32, 32, 16, 16, 16)
  ))
  for (fun in list(transform_clr, transform_alr, auto_coda)) {
    ref <- suppressMessages(fun(x, gamma = 0.2, seed = 41))
    for (scale in known_scales) {
      input <- scale_input(x, scale)
      out <- suppressMessages(fun(input, gamma = 0.2, seed = 41))
      expect_identical(.get_scale(out), "logx")
      expect_equal(.get_expr_mat(out), .get_expr_mat(ref))
      expect_identical(apply_pipeline(input, get_pipeline(out)), out)
      expect_error(fun(out), "logx")
    }
  }
  x <- scale_input(scale_test_matrix(), "log10")
  raw <- scale_test_matrix()
  ref <- normalize_vsn(raw, seed = 42)
  out <- normalize_vsn(x, seed = 42)
  expect_equal(.get_expr_mat(out), .get_expr_mat(ref), tolerance = 1e-4)
  expect_identical(.get_scale(out), "logx")
  expect_identical(apply_pipeline(x, get_pipeline(out)), out)
})

test_that("batch detection and correction share offset log working domain", {
  x <- scale_test_matrix()
  for (method in c("limma", "combat")) {
    ref <- correct_batch_effect(x, method = method, check_confounding = FALSE)
    p <- suppressMessages(detect_batch_effect(x, group = "group"))
    for (scale in known_scales) {
      input <- scale_input(x, scale)
      out <- correct_batch_effect(
        input,
        method = method,
        check_confounding = FALSE
      )
      expect_identical(.get_scale(out), scale)
      expect_equal(
        .to_work_scale(.get_expr_mat(out), scale),
        .get_expr_mat(ref),
        tolerance = 1e-7
      )
      expect_equal(
        suppressMessages(detect_batch_effect(input, group = "group")),
        p
      )
      expect_identical(apply_pipeline(input, get_pipeline(out)), out)
    }
  }
  input <- scale_input(x, "log2")
  mat <- .get_expr_mat(input) - 5
  meta <- .get_container_metadata(input)
  meta$scale <- "logx"
  input <- .rebuild_container(input, expr_mat = mat, metadata = meta)
  expected <- limma::removeBatchEffect(
    mat,
    batch = .get_sample_info(input)$batch
  )
  out <- correct_batch_effect(
    input,
    method = "limma",
    check_confounding = FALSE
  )
  expect_equal(.get_expr_mat(out), expected)
  expect_true(any(.get_expr_mat(out) < 0))
  expect_equal(
    suppressMessages(detect_batch_effect(input)),
    suppressMessages(.perform_batch_effect_detection(
      mat,
      .get_sample_info(input)$batch
    ))
  )
})

test_that("batch no-op paths retain the original representation exactly", {
  x <- scale_test_matrix()
  for (scale in c(known_scales, "logx")) {
    input <- if (scale == "logx") {
      scale_input(x, "log2")
    } else {
      scale_input(x, scale)
    }
    if (scale == "logx") {
      S4Vectors::metadata(input)$scale <- "logx"
    }
    mat <- .get_expr_mat(input)
    out <- suppressWarnings(correct_batch_effect(input, batch = rep("A", 8)))
    expect_identical(.get_expr_mat(out), mat)
    out <- suppressWarnings(correct_batch_effect(input, batch = seq_len(8)))
    expect_identical(.get_expr_mat(out), mat)
    out <- suppressWarnings(correct_batch_effect(input, group = "batch"))
    expect_identical(.get_expr_mat(out), mat)
  }
  local_mocked_bindings(.apply_batch_correction = function(...) NULL)
  input <- scale_input(x, "log10")
  out <- correct_batch_effect(input)
  expect_identical(.get_expr_mat(out), .get_expr_mat(input))
})

test_that("QC derives raw summaries and log coordinates from scale metadata", {
  x <- scale_test_matrix()
  funcs <- list(
    plot_tic_bar,
    plot_cv_dent,
    plot_rank_abundance,
    plot_int_boxplot,
    plot_rle,
    plot_missing_bar
  )
  for (fun in funcs) {
    ref <- fun(x)$data
    for (scale in known_scales) {
      expect_equal(fun(scale_input(x, scale))$data, ref)
    }
  }
  input <- scale_input(x, "log2")
  mat <- .get_expr_mat(input) - 5
  mat[1, 1] <- 0
  S4Vectors::metadata(input)$scale <- "logx"
  SummarizedExperiment::assay(input) <- mat
  plot <- plot_int_boxplot(input)
  expect_equal(sort(plot$data$log_intensity), sort(c(mat)))
  expect_identical(plot$labels$y, "Log-like coordinate")
  rle <- plot_rle(input)
  expect_equal(
    sort(rle$data$rle),
    sort(c(sweep(mat, 1, matrixStats::rowMedians(mat), "-")))
  )
  expect_s3_class(plot_batch_pca(input), "ggplot")
  expect_s3_class(
    plot_rep_scatter(input, "replicate", n_pairs = 1),
    "patchwork"
  )
})

test_that("containers default absent scale to raw but reject explicit invalid scales", {
  inputs <- list(
    scale_test_matrix(),
    plain_se(scale_test_matrix()),
    legacy_exp(60, 8)
  )
  for (input in inputs) {
    for (scale in known_scales) {
      x <- scale_input(input, scale)
      out <- normalize_total_area(x)
      expect_identical(class(out), class(x))
      expect_identical(.get_scale(out), scale)
      expect_equal(
        colSums(.to_work_scale(.get_expr_mat(out), scale)),
        rep(1, 8),
        ignore_attr = TRUE
      )
    }
    meta <- .get_container_metadata(input)
    meta$scale <- NULL
    x <- .rebuild_container(input, metadata = meta)
    expect_equal(
      .get_expr_mat(normalize_total_area(x)),
      .get_expr_mat(normalize_total_area(input))
    )
  }
  x <- plain_se(scale_test_matrix())
  S4Vectors::metadata(x)$scale <- "invalid"
  expect_error(normalize_total_area(x), "scale|choices|element")
  expect_error(remove_rare(x), "scale|choices|element")
})

test_that("stable known-log operations handle extreme logs and zeros", {
  x <- scale_input(scale_test_matrix(4, 8), "log2")
  mat <- .get_expr_mat(x) + 2000
  SummarizedExperiment::assay(x) <- mat
  out <- normalize_total_area(x)
  expect_true(all(is.finite(.get_expr_mat(out))))
  expect_equal(colSums(2^.get_expr_mat(out)), rep(1, 8), ignore_attr = TRUE)
  for (fun in list(
    normalize_median,
    normalize_median_abs,
    normalize_median_quotient,
    function(x) transform_clr(x, gamma = 0)
  )) {
    expect_true(all(is.finite(.get_expr_mat(fun(x)))))
  }
  out <- aggregate(x, to_level = "g", standardize_variable = FALSE)
  expect_true(all(is.finite(.get_expr_mat(out))))
  expect_equal(.get_expr_mat(out)[1, 1], .log2_sum(mat[, 1]))
  mat[, 1] <- -2000
  mat[1, 2] <- -Inf
  SummarizedExperiment::assay(x) <- mat
  expect_equal(
    .get_expr_mat(normalize_total_area(x))[, 1],
    rep(-2, 4),
    ignore_attr = TRUE
  )
  expect_equal(
    .batch_work_matrix(x)[, 1],
    rep(log2(1e-6), 4),
    ignore_attr = TRUE
  )
  y <- zero_to_na(x)
  expect_true(is.na(.get_expr_mat(y)[1, 2]))
  z <- remove_constant(x)
  expect_identical(.get_expr_mat(z), mat[rownames(z), , drop = FALSE])
  mat[,] <- -Inf
  SummarizedExperiment::assay(x) <- mat
  expect_equal(nrow(suppressMessages(remove_constant(x))), 0L)
})

test_that("single-column normalization and half-min preserve zero abundance semantics", {
  x <- test_glycomic_se(matrix(c(0, 0, 4), 3))
  for (scale in known_scales) {
    input <- scale_input(x, scale)
    for (fun in list(normalize_median, normalize_median_abs)) {
      expect_identical(.get_expr_mat(fun(input)), .get_expr_mat(input))
    }
    y <- scale_input(test_glycomic_se(matrix(c(0, NA), 2)), scale)
    out <- impute_half_sample_min(y)
    expected <- .from_work_scale(
      matrix(c(0, 0), 2, dimnames = dimnames(.get_expr_mat(y))),
      scale
    )
    expect_identical(.get_expr_mat(out), expected)
    expect_error(
      normalize_median_quotient(
        scale_input(scale_test_matrix(), scale),
        by = "sample"
      ),
      "Can't stratify"
    )
  }
  for (fun in list(transform_clr, transform_alr, auto_coda)) {
    input <- test_glycomic_se(matrix(c(1, 2), 2))
    out <- suppressWarnings(suppressMessages(fun(input, gamma = 0)))
    expect_equal(.get_expr_mat(out)[, 1], c(-0.5, 0.5), ignore_attr = TRUE)
    expect_identical(.get_scale(out), "logx")
    expect_identical(apply_pipeline(input, get_pipeline(out)), out)
  }
})

test_that("explicit invalid scale is not treated as absent scale", {
  for (value in list(NULL, NA_character_, c("raw", "log2"), list("raw"))) {
    x <- plain_se(scale_test_matrix())
    meta <- .get_container_metadata(x)
    meta["scale"] <- list(value)
    S4Vectors::metadata(x) <- meta
    expect_error(normalize_total_area(x))
  }
})

test_that("site-wise CLR is equivalent on all ordinary scales", {
  x <- scale_test_matrix(12, 8)
  rd <- SummarizedExperiment::rowData(x)
  rd$protein <- rep(c("P1", "P2"), each = 6)
  rd$protein_site <- rep(rep(c(10L, 20L), each = 3), 2)
  SummarizedExperiment::rowData(x) <- rd
  x <- glyexp::as_glycoproteomic_se(x)
  for (group_scales in list(NULL, 2, c(A = 1, B = 2))) {
    ref <- transform_clr(
      x,
      by = "group",
      group_scales = group_scales,
      seed = 91
    )
    for (scale in known_scales) {
      input <- scale_input(x, scale)
      out <- transform_clr(
        input,
        by = "group",
        group_scales = group_scales,
        seed = 91
      )
      expect_equal(.get_expr_mat(out), .get_expr_mat(ref))
      expect_identical(.get_scale(out), "logx")
      expect_identical(.get_var_info(out), .get_var_info(input))
      expect_identical(apply_pipeline(input, get_pipeline(out)), out)
    }
  }
})

test_that("batch inverse offset preserves zero abundance and direct coordinates", {
  work <- matrix(c(log2(1e-6) - 2, log2(1e-6), log2(1e-6) + 1, 12), 2)
  raw <- pmax(2^work - 1e-6, 0)
  for (scale in known_scales) {
    out <- .batch_from_work(work, scale)
    expect_equal(.to_work_scale(out, scale), raw, tolerance = 1e-12)
    if (scale != "raw") expect_identical(out[1, 1], -Inf)
  }
  expect_identical(.batch_from_work(work, "raw")[2, 1], 0)
  expect_identical(.batch_from_work(work, "logx"), work)
  extreme <- matrix(c(-2000, 2000), 2)
  expect_true(all(is.finite(.batch_from_work(
    extreme[2, , drop = FALSE],
    "log2"
  ))))
})

test_that("processing and replay preserve scale across legacy and plain containers", {
  x <- scale_test_matrix(12, 8)
  legacy <- legacy_exp(12, 8)
  legacy$expr_mat <- .get_expr_mat(x)
  legacy$sample_info <- .get_sample_info(x)
  inputs <- list(x, plain_se(x), legacy)
  for (input in inputs) {
    for (scale in known_scales) {
      original <- scale_input(input, scale)
      out <- original |>
        normalize_total_area() |>
        impute_min_prob(seed = 19) |>
        remove_constant() |>
        correct_batch_effect(method = "limma", check_confounding = FALSE)
      expect_identical(class(out), class(original))
      expect_identical(.get_scale(out), scale)
      expect_identical(apply_pipeline(original, get_pipeline(out)), out)
      coda <- transform_clr(original, gamma = 0)
      expect_identical(class(coda), class(original))
      expect_identical(.get_scale(coda), "logx")
      expect_identical(apply_pipeline(original, get_pipeline(coda)), coda)
    }
  }
})

test_that("log-domain algorithms explain how to handle zero abundances", {
  x <- test_glycomic_se(matrix(c(0, 1, 2, NA, 4, 8), 3))
  for (scale in known_scales) {
    input <- scale_input(x, scale)
    expect_error(impute_min_prob(input), "zero_to_na")
    expect_error(normalize_loessf(input), "zero_to_na")
    expect_error(transform_clr(input), "zero_to_na|Missing values")
    out <- impute_half_sample_min(zero_to_na(input))
    expect_true(all(is.finite(.get_expr_mat(transform_clr(out, gamma = 0)))))
  }
})

test_that("batch correction preserves a zero row when other rows change", {
  mat <- rbind(zero = rep(0, 8), variable = c(1, 2, 3, 4, 20, 30, 40, 50))
  x <- test_glycomic_se(mat)
  for (scale in known_scales) {
    input <- scale_input(x, scale)
    out <- correct_batch_effect(
      input,
      batch = rep(c("a", "b"), each = 4),
      method = "limma",
      check_confounding = FALSE
    )
    expect_identical(
      .to_work_scale(.get_expr_mat(out), scale)[1, ],
      stats::setNames(rep(0, 8), colnames(.get_expr_mat(input)))
    )
    expect_false(isTRUE(all.equal(
      .get_expr_mat(out)[2, ],
      .get_expr_mat(input)[2, ]
    )))
  }
})

test_that("even median quotient handles zero references in abundance units", {
  x <- test_glycomic_se(matrix(c(0, 0, 0, 0, 1, 2), 2))
  ref <- normalize_median_quotient(x)
  for (scale in known_scales) {
    out <- normalize_median_quotient(scale_input(x, scale))
    expect_equal(.to_work_scale(.get_expr_mat(out), scale), .get_expr_mat(ref))
    expect_identical(
      .to_work_scale(.get_expr_mat(out), scale)[, 3],
      stats::setNames(c(0, 0), rownames(x))
    )
  }
})
