# Inspect the text through a fixed, non-interactive cli connection, independent
# of whether testthat runs in an interactive IDE or a package-check process.
capture_pipeline_print <- function(x, width = 80, colors = 1) {
  withr::local_options(list(cli.num_colors = colors, cli.unicode = FALSE,
    cli.width = width, width = width, cli.dynamic = FALSE))
  result <- NULL
  output <- capture.output({
    cli::start_app(output = "stdout", .auto_close = TRUE, .envir = environment())
    result <- withVisible(print(x))
  })
  list(text = paste(output, collapse = "\n"), lines = output, result = result)
}

print_test_pipeline <- function(parameters, fun = transform_clr) {
  structure(list(structure(list(`function` = fun, parameters = parameters),
    class = c("glyclean_step", "list"))), class = c("glyclean_pipeline", "list"))
}

test_that("pipeline print uses cli headings, lists and wrapped parameter text", {
  pipeline <- glyclean_pipeline(
    normalize_total_area(),
    impute_min_prob(seed = 42)
  )
  expect_snapshot(cat(
    capture_pipeline_print(pipeline, width = 60)$text,
    "\n",
    sep = ""
  ))
  literal <- "{.strong literal}"
  shown <- capture_pipeline_print(print_test_pipeline(list(value = literal)))
  expect_match(shown$text, literal, fixed = TRUE)
})

test_that("pipeline print shows its class, step count, names and scalar seeds", {
  pipeline <- glyclean_pipeline(normalize_total_area(), impute_min_prob(seed = 42))
  shown <- capture_pipeline_print(pipeline)
  expect_match(shown$text, "<glyclean_pipeline>.*2 steps")
  expect_match(shown$text, "1\\. normalize_total_area")
  expect_match(shown$text, "2\\. impute_min_prob")
  expect_match(shown$text, "seed = 42")
  expect_false(shown$result$visible)
  expect_identical(shown$result$value, pipeline)
  single <- capture_pipeline_print(glyclean_pipeline(transform_clr()))
  expect_match(single$text, "1 step")
  expect_match(single$text, "seed = 123")
})

test_that("recorded history retains operation names across namespace reloads", {
  x <- simple_exp(4, 3)
  SummarizedExperiment::colData(x)$group <- factor(c("A", "A", "B"))
  result <- suppressMessages(remove_rare(
    x,
    prop = 0.5,
    by = "group",
    min_n = 1
  ))
  result <- normalize_total_area(result)
  result <- impute_min_prob(result)
  result <- glyclean::aggregate(result, to_level = "g")
  history <- get_pipeline(result)
  # An installed package and load_all() have distinct environments and source
  # references, even when their operation implementations are the same.
  for (i in seq_along(history)) {
    fun <- rlang::zap_srcref(history[[i]][["function"]])
    environment(fun) <- new.env(parent = environment(fun))
    history[[i]][["function"]] <- fun
  }
  expect_false(identical(history[[1]][["function"]], remove_rare))
  before <- serialize(history, NULL)
  shown <- capture_pipeline_print(unserialize(before), width = 120)
  for (name in c(
    "remove_rare",
    "normalize_total_area",
    "impute_min_prob",
    "aggregate"
  )) {
    expect_match(shown$text, name, fixed = TRUE)
  }
  expect_false(grepl("recorded operation", shown$text, fixed = TRUE))
  expect_match(shown$text, 'by = <factor [3]: "A" (2), "B" (1)>', fixed = TRUE)
  expect_match(shown$text, "f = base::sum", fixed = TRUE)
  expect_identical(serialize(history, NULL), before)
})

test_that("callback summaries identify named functions without executing them", {
  pipeline <- print_test_pipeline(list(
    f = base::sum,
    average = base::mean,
    middle = stats::median,
    internal = .pipeline_function_names,
    callback = function(x, ...) stop("must not execute")
  ))
  shown <- capture_pipeline_print(pipeline, width = 120)
  expect_match(shown$text, "f = base::sum", fixed = TRUE)
  expect_match(shown$text, "average = base::mean", fixed = TRUE)
  expect_match(shown$text, "middle = stats::median", fixed = TRUE)
  expect_match(
    shown$text,
    "internal = glyclean:::.pipeline_function_names",
    fixed = TRUE
  )
  expect_match(shown$text, "callback = function(x, ...)", fixed = TRUE)
  expect_false(grepl("must not execute", shown$text, fixed = TRUE))
})

test_that("factor summaries show bounded group counts including missing values", {
  expect_identical(.pipeline_factor_summary(factor()), "<factor [0]>")
  expect_identical(
    .pipeline_factor_summary(factor(c(NA, NA))),
    "<factor [2]: NA (2)>"
  )
  groups <- factor(c("A", "A", "B", NA), levels = c("A", "B", "unused"))
  shown <- capture_pipeline_print(
    print_test_pipeline(list(by = groups)),
    width = 120
  )
  expect_match(
    shown$text,
    'by = <factor [4]: "A" (2), "B" (1), NA (1)>',
    fixed = TRUE
  )
  expect_false(grepl("unused", shown$text, fixed = TRUE))
  groups <- factor(paste0("group", seq_len(100)))
  shown <- capture_pipeline_print(
    print_test_pipeline(list(by = groups)),
    width = 120
  )
  expect_match(shown$text, "factor [100]", fixed = TRUE)
  expect_match(shown$text, "...", fixed = TRUE)
  expect_false(grepl('"group99"', shown$text, fixed = TRUE))
})

test_that("pipeline print summarizes matrices, lists and sequences without dumping", {
  marker <- "DO_NOT_DUMP_PRIVATE_PARAMETER_CONTENT"
  pipeline <- print_test_pipeline(list(
    mat = matrix(marker, nrow = 60, ncol = 6),
    values = rep(marker, 100),
    nested = list(marker, list(marker), marker),
    sequence = paste(rep("ACDEFGHIKLMNPQRSTVWY", 20), collapse = ""),
    callback = function() stop("must not execute")
  ))
  shown <- capture_pipeline_print(pipeline, width = 120)
  expect_match(shown$text, "<matrix \\[60 x 6\\]>")
  expect_match(shown$text, "<character \\[100\\]>")
  expect_match(shown$text, "<list \\[3\\]>")
  expect_match(shown$text, "<character \\[1\\]>")
  expect_match(shown$text, "function()", fixed = TRUE)
  expect_false(grepl(marker, shown$text, fixed = TRUE))
  expect_false(grepl("ACDEFGHIKLMNPQRSTVWY", shown$text, fixed = TRUE))
  expect_false(grepl("must not execute", shown$text, fixed = TRUE))
})

test_that("empty pipelines print explicitly and return invisibly", {
  pipeline <- glyclean_pipeline()
  shown <- capture_pipeline_print(pipeline)
  expect_match(shown$text, "<glyclean_pipeline>.*0 steps")
  expect_match(shown$text, "empty")
  expect_false(shown$result$visible)
  expect_identical(shown$result$value, pipeline)
})

test_that("pipeline print respects narrow terminals and has no noninteractive ANSI", {
  pipeline <- glyclean_pipeline(transform_clr(gamma = 0.2, seed = 87),
                                 impute_min_prob(seed = 712))
  for (width in c(20L, 40L)) {
    shown <- capture_pipeline_print(pipeline, width = width)
    expect_true(all(nchar(shown$lines, type = "width") <= width))
    expect_identical(shown$result$value, pipeline)
  }
  # Package checks are non-interactive; even a color-capable caller should get
  # clean log text, without leaking the local cli option override to the caller.
  if (!interactive()) {
    withr::local_options(cli.num_colors = 256)
    shown <- capture_pipeline_print(pipeline, colors = 256)
    expect_false(grepl("\033", shown$text, fixed = TRUE))
    expect_identical(getOption("cli.num_colors"), 256)
  }
})

test_that("printing does not execute functions or mutate the complete object", {
  dangerous <- function(...) stop("stored function was executed")
  environment(dangerous) <- baseenv()
  pipeline <- print_test_pipeline(list(seed = 22, callback = dangerous), fun = dangerous)
  attr(pipeline, "custom") <- list(keep = "unchanged")
  attr(pipeline[[1L]], "package_version") <- "test-version"
  before <- serialize(pipeline, NULL)
  shown <- capture_pipeline_print(pipeline)
  expect_match(shown$text, "recorded operation")
  expect_false(grepl("stored function was executed", shown$text, fixed = TRUE))
  expect_identical(serialize(pipeline, NULL), before)
  expect_identical(serialize(shown$result$value, NULL), before)
  expect_false(shown$result$visible)
})

test_that("pipeline print does not dispatch custom parameter formatting methods", {
  fail <- function(...) stop("custom parameter method was executed")
  rlang::local_bindings(
    print.dangerous_pipeline_value = fail,
    format.dangerous_pipeline_value = fail,
    as.character.dangerous_pipeline_value = fail,
    length.dangerous_pipeline_value = fail,
    .env = .GlobalEnv
  )
  value <- structure("DO_NOT_DUMP_CUSTOM_VALUE", class = "dangerous_pipeline_value")
  shown <- capture_pipeline_print(print_test_pipeline(list(value = value)))
  expect_match(shown$text, "<dangerous_pipeline_value>")
  expect_false(grepl("DO_NOT_DUMP_CUSTOM_VALUE", shown$text, fixed = TRUE))
})
