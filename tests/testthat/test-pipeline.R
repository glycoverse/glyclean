pipeline_test_exp <- function() {
  mat <- matrix(
    c(1, 2, 4, 8, 3, 6, 12, 24, 2, 5, 10, 20),
    nrow = 4,
    dimnames = list(paste0("V", 1:4), paste0("S", 1:3))
  )
  SummarizedExperiment::SummarizedExperiment(
    assays = list(signal = mat),
    rowData = S4Vectors::DataFrame(feature = letters[1:4]),
    colData = S4Vectors::DataFrame(group = c("A", "A", "B")),
    metadata = list(custom = list(keep = TRUE))
  )
}

expect_pipeline_result <- function(actual, expected) {
  expect_identical(class(actual), class(expected))
  expect_equal(SummarizedExperiment::assay(actual),
               SummarizedExperiment::assay(expected))
  expect_identical(SummarizedExperiment::assayNames(actual),
                   SummarizedExperiment::assayNames(expected))
  expect_identical(SummarizedExperiment::rowData(actual),
                   SummarizedExperiment::rowData(expected))
  expect_identical(SummarizedExperiment::colData(actual),
                   SummarizedExperiment::colData(expected))
  actual_meta <- S4Vectors::metadata(actual)
  expected_meta <- S4Vectors::metadata(expected)
  actual_meta$pipeline <- NULL
  expected_meta$pipeline <- NULL
  expect_equal(actual_meta, expected_meta)
}

test_that("pipeline is a list of two-field function-and-parameter steps", {
  pipeline <- glyclean_pipeline(
    normalize_total_area(),
    impute_min_prob(q = 0.05, seed = 81)
  )
  expect_type(pipeline, "list")
  expect_s3_class(pipeline, "glyclean_pipeline")
  expect_length(pipeline, 2L)
  for (step in pipeline) {
    expect_type(step, "list")
    expect_s3_class(step, "glyclean_step")
    expect_identical(names(step), c("function", "parameters"))
    expect_type(step[["function"]], "closure")
    expect_type(step$parameters, "list")
  }
  expect_identical(pipeline[[1]][["function"]], normalize_total_area)
  expect_identical(pipeline[[2]][["function"]], impute_min_prob)
  expect_identical(pipeline[[2]]$parameters$q, 0.05)
  expect_identical(pipeline[[2]]$parameters$seed, 81)
})

test_that("construction captures calls without running transformations", {
  # These calls require an experiment when evaluated normally.
  expect_no_error(pipeline <- glyclean_pipeline(
    normalize_total_area(), remove_rare(prop = 0.5)
  ))
  expect_length(pipeline, 2L)
  expect_length(glyclean_pipeline(), 0L)
  expect_error(glyclean_pipeline(normalize_total_area))
})

test_that("step arguments must be explicitly named and valid", {
  expect_error(glyclean_pipeline(remove_rare(0.5)))
  expect_error(glyclean_pipeline(remove_rare(prop = 0.5, prop = 0.8)))
  expect_error(glyclean_pipeline(normalize_total_area(unknown = 1)))
  expect_error(glyclean_pipeline(normalize_total_area(x = pipeline_test_exp())))
  expect_error(glyclean_pipeline(1))
  expect_error(glyclean_pipeline(remove_rare(pro = 0.5)))
})

test_that("namespace-qualified processing calls are captured", {
  pipeline <- glyclean_pipeline(glyclean::normalize_total_area())
  expect_length(pipeline, 1L)
  expect_identical(pipeline[[1]][["function"]], normalize_total_area)
  x <- pipeline_test_exp()
  expect_pipeline_result(apply_pipeline(x, pipeline), normalize_total_area(x))
})

test_that("supplied parameter expressions are evaluated and frozen once", {
  q <- 0.1
  evaluations <- 0L
  pipeline <- glyclean_pipeline(impute_min_prob(
    q = { evaluations <- evaluations + 1L; q }, seed = 9
  ))
  expect_identical(evaluations, 1L)
  q <- 0.9
  expect_identical(pipeline[[1]]$parameters$q, 0.1)
  expect_identical(pipeline[[1]]$parameters$seed, 9)
  x <- pipeline_test_exp()
  SummarizedExperiment::assay(x)[1, 1] <- NA_real_
  actual <- apply_pipeline(x, pipeline)
  expected <- impute_min_prob(x, q = 0.1, seed = 9)
  expect_pipeline_result(actual, expected)
  expect_identical(evaluations, 1L)
})

test_that("manual processing appends history while retaining one assay", {
  x <- pipeline_test_exp()
  original <- x
  expect_s3_class(get_pipeline(x), "glyclean_pipeline")
  expect_length(get_pipeline(x), 0L)
  first <- normalize_total_area(x)
  second <- normalize_median(first)
  history <- get_pipeline(second)
  expect_length(history, 2L)
  expect_identical(history[[1]][["function"]], normalize_total_area)
  expect_identical(history[[2]][["function"]], normalize_median)
  expect_identical(S4Vectors::metadata(second)$pipeline, history)
  expect_identical(S4Vectors::metadata(second)$custom, list(keep = TRUE))
  expect_identical(SummarizedExperiment::assayNames(second), "signal")
  expect_length(SummarizedExperiment::assays(second), 1L)
  expect_identical(x, original)
  expect_length(get_pipeline(first), 1L)
})

test_that("applying a pipeline does not duplicate built-in step records", {
  x <- pipeline_test_exp()
  pipeline <- glyclean_pipeline(normalize_total_area(), impute_zero())
  result <- apply_pipeline(x, pipeline)
  expect_length(get_pipeline(result), 2L)
  expect_pipeline_result(result, impute_zero(normalize_total_area(x)))
  expect_length(get_pipeline(x), 0L)
  with_history <- normalize_median(x)
  result <- apply_pipeline(with_history, pipeline)
  expect_length(get_pipeline(result), 3L)
  expect_identical(get_pipeline(result)[[1]][["function"]], normalize_median)
  expect_identical(apply_pipeline(x, glyclean_pipeline()), x)
})

test_that("history freezes defaults and reproduces seeded imputation", {
  x <- pipeline_test_exp()
  SummarizedExperiment::assay(x)[c(1, 7)] <- NA_real_
  original <- x
  result <- impute_min_prob(x)
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(history[[1]]$parameters$seed, 123)
  expect_identical(history[[1]]$parameters$q, 0.01)
  expect_identical(history[[1]]$parameters$tune.sigma, 1)
  set.seed(321)
  rng_before <- .Random.seed
  replay <- apply_pipeline(x, history)
  expect_pipeline_result(replay, result)
  expect_identical(.Random.seed, rng_before)
  expect_length(get_pipeline(replay), 1L)
  expect_identical(x, original)
})

test_that("filtering history reproduces row alignment and class", {
  x <- pipeline_test_exp()
  SummarizedExperiment::assay(x)[1, ] <- NA_real_
  original <- x
  result <- suppressMessages(remove_rare(x, prop = 0.5, min_n = 1))
  expect_identical(rownames(result), c("V2", "V3", "V4"))
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(history[[1]][["function"]], remove_rare)
  replay <- suppressMessages(apply_pipeline(x, history))
  expect_pipeline_result(replay, result)
  expect_identical(x, original)
})

test_that("automatic normalization records its concrete method", {
  x <- simple_exp(4, 3)
  result <- suppressMessages(auto_normalize(x))
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(history[[1]][["function"]], normalize_total_area)
  expect_pipeline_result(apply_pipeline(x, history), result)
})

test_that("automatic filtering retains both concrete steps on returned input", {
  x <- pipeline_test_exp()
  SummarizedExperiment::assay(x)[1, ] <- NA_real_
  result <- suppressMessages(auto_remove(x, preset = "discovery"))
  history <- get_pipeline(result)
  expect_length(history, 2L)
  expect_true(all(vapply(history, function(step) {
    identical(step[["function"]], remove_rare)
  }, logical(1))))
  expect_identical(history[[1]]$parameters$prop, 0.8)
  expect_identical(history[[2]]$parameters$prop, 0.5)
  replay <- suppressMessages(apply_pipeline(x, history))
  expect_pipeline_result(replay, result)
})

test_that("automatic filtering freezes effective groups with unused factor levels", {
  x <- pipeline_test_exp()
  SummarizedExperiment::colData(x)$group <- factor(
    c("A", "A", "B"), levels = c("A", "B", "unused")
  )
  SummarizedExperiment::assay(x)[1, ] <- NA_real_
  result <- suppressMessages(auto_remove(x, preset = "discovery"))
  history <- get_pipeline(result)
  expect_length(history, 2L)
  expect_identical(history[[2]]$parameters$by,
                   factor(c("A", "A", "B"), levels = c("A", "B")))
  expect_identical(levels(SummarizedExperiment::colData(result)$group),
                   c("A", "B", "unused"))
  replay <- suppressMessages(apply_pipeline(x, history))
  expect_pipeline_result(replay, result)
})

test_that("pipeline and recorded history survive RDS serialization", {
  x <- pipeline_test_exp()
  SummarizedExperiment::assay(x)[2, 2] <- NA_real_
  pipeline <- glyclean_pipeline(
    normalize_total_area(), impute_min_prob(seed = 471)
  )
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(pipeline, file)
  restored <- readRDS(file)
  expect_equal(restored, pipeline)
  result <- apply_pipeline(x, restored)
  saveRDS(result, file)
  restored_result <- readRDS(file)
  replay <- apply_pipeline(x, get_pipeline(restored_result))
  expect_pipeline_result(replay, result)
  expect_length(get_pipeline(replay), 2L)
})

test_that("errors leave original object and its existing history unchanged", {
  x <- normalize_total_area(pipeline_test_exp())
  original <- x
  pipeline <- glyclean_pipeline(
    impute_zero(), impute_min_prob(q = -1)
  )
  expect_error(apply_pipeline(x, pipeline))
  expect_identical(x, original)
  expect_length(get_pipeline(x), 1L)
  expect_error(impute_min_prob(x, q = -1))
  expect_identical(x, original)
})

test_that("malformed pipeline records are rejected rather than overwritten", {
  x <- pipeline_test_exp()
  malformed <- glyclean_pipeline(normalize_total_area())
  malformed[[1]][["function"]] <- "normalize_total_area"
  expect_error(apply_pipeline(x, malformed))
  malformed <- glyclean_pipeline(normalize_total_area())
  malformed[[1]]$parameters <- 1
  expect_error(apply_pipeline(x, malformed))

  S4Vectors::metadata(x)$pipeline <- "user metadata with incompatible schema"
  original <- x
  expect_error(get_pipeline(x))
  expect_error(normalize_total_area(x))
  expect_identical(x, original)
})

test_that("legacy experiment metadata retains replayable history", {
  x <- suppressWarnings(legacy_exp(4, 3))
  original <- x
  result <- normalize_total_area(x)
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(result$meta_data$pipeline, history)
  replay <- apply_pipeline(x, history)
  expect_equal(replay$expr_mat, result$expr_mat)
  expect_identical(replay$sample_info, result$sample_info)
  expect_identical(replay$var_info, result$var_info)
  expect_identical(class(replay), class(x))
  expect_identical(x, original)
})

test_that("aggregation records its resolved level and frozen callback", {
  x <- simple_exp(4, 3)
  callback_env <- new.env(parent = baseenv())
  callback_env$offset <- 2
  callback <- eval(quote(function(values) sum(values) + offset), callback_env)
  result <- aggregate(x, f = callback)
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(history[[1]][["function"]], aggregate)
  expect_identical(history[[1]]$parameters$to_level, "g")
  expect_identical(history[[1]]$parameters$standardize_variable, TRUE)
  expect_true(is.function(history[[1]]$parameters$f))
  expect_identical(history[[1]]$parameters$f(c(1, 2)), 5)

  callback_env$offset <- 100
  expect_identical(callback(c(1, 2)), 103)
  expect_identical(history[[1]]$parameters$f(c(1, 2)), 5)
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(history, file)
  restored <- readRDS(file)
  expect_pipeline_result(apply_pipeline(x, restored), result)

  summed <- aggregate(x)
  expect_identical(get_pipeline(summed)[[1]]$parameters$f, sum)
  expect_pipeline_result(apply_pipeline(x, get_pipeline(summed)), summed)
})

pipeline_protein_exp <- function() {
  mat <- matrix(
    c(10, 20, 20, 40, 30, 60), nrow = 2,
    dimnames = list(c("V1", "V2"), c("S1", "S2", "S3"))
  )
  test_glycoproteomic_se(
    mat,
    var_info = tibble::tibble(
      variable = c("V1", "V2"),
      protein = c("P12345", "Q67890"),
      protein_site = c(10L, 5L),
      glycan_composition = c("H5N2", "H3N2")
    )
  )
}

test_that("site annotation freezes FASTA contents rather than its file path", {
  x <- pipeline_protein_exp()
  fasta <- tempfile(fileext = ".fasta")
  on.exit(unlink(fasta), add = TRUE)
  writeLines(c(
    ">sp|P12345|TEST1_HUMAN Test protein 1",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    ">sp|Q67890|TEST2_HUMAN Test protein 2",
    "MNPQRSTUVW"
  ), fasta)
  result <- suppressMessages(add_site_seq(x, fasta = fasta, n_aa = 3))
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(history[[1]][["function"]], add_site_seq)
  expect_identical(history[[1]]$parameters$n_aa, 3)
  expect_equal(history[[1]]$parameters$fasta, c(
    P12345 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", Q67890 = "MNPQRSTUVW"
  ))
  unlink(fasta)
  expect_false(file.exists(fasta))
  replay <- suppressMessages(apply_pipeline(x, history))
  expect_pipeline_result(replay, result)
  expect_identical(SummarizedExperiment::rowData(replay)$site_sequence,
                   c("GHIJKLM", "NPQRSTU"))
})

test_that("protein adjustment records its selected method and matrix values", {
  x <- pipeline_protein_exp()
  proteins <- matrix(
    c(1, 2, 2, 4, 3, 6), nrow = 2,
    dimnames = list(c("P12345", "Q67890"), c("S1", "S2", "S3"))
  )
  expected_proteins <- proteins
  result <- adjust_protein(x, pro_expr_mat = proteins)
  history <- get_pipeline(result)
  expect_length(history, 1L)
  expect_identical(history[[1]][["function"]], adjust_protein)
  expect_identical(history[[1]]$parameters$method, "ratio")
  expect_identical(history[[1]]$parameters$pro_expr_mat, expected_proteins)
  proteins[] <- 999
  expect_identical(history[[1]]$parameters$pro_expr_mat, expected_proteins)
  expect_pipeline_result(apply_pipeline(x, history), result)
})

test_that("recipes freeze defaults and reject missing external inputs", {
  recipe <- glyclean_pipeline(impute_min_prob())
  expect_identical(recipe[[1]]$parameters$seed, 123)
  expect_identical(recipe[[1]]$parameters$q, 0.01)
  expect_error(glyclean_pipeline(adjust_protein()), "Missing required")
  expect_error(glyclean_pipeline(aggregate(f = function(x) {
    sum(x) + get("offset")
  })), "static lexical")
})

test_that("positional forwarded arguments remain supported in direct calls", {
  skip_if_not_installed("pcaMethods")
  mat <- matrix(seq_len(100), nrow = 10,
                dimnames = list(paste0("v", 1:10), paste0("s", 1:10)))
  mat[c(1, 5, 29, 68, 45, 100)] <- NA_real_
  x <- SummarizedExperiment::SummarizedExperiment(assays = list(abundance = mat))
  result <- suppressWarnings(impute_ppca(x, NULL, "none", seed = 42))
  history <- get_pipeline(result)
  expect_identical(history[[1]]$parameters$scale, "none")
  expect_pipeline_result(suppressWarnings(apply_pipeline(x, history)), result)
  expect_error(glyclean_pipeline(impute_ppca(NULL, "none")))
})

test_that("single-sample character grouping values are unambiguous on replay", {
  x <- SummarizedExperiment::SummarizedExperiment(
    assays = list(signal = matrix(c(1, 2, 3), ncol = 1,
      dimnames = list(c("V1", "V2", "V3"), "S1"))),
    colData = S4Vectors::DataFrame(group = "A")
  )
  for (column_collision in c(FALSE, TRUE)) {
    if (column_collision) SummarizedExperiment::colData(x)$A <- "B"
    result <- remove_rare(x, prop = 0.5, by = "group")
    history <- get_pipeline(result)
    expect_identical(history[[1L]]$parameters$by, factor("A"))
    expect_identical(apply_pipeline(x, history), result)
    expect_identical(apply_pipeline(x, unserialize(serialize(history, NULL))), result)
  }
})
