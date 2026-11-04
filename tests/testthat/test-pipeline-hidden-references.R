reference_callback <- function(config) {
  env <- new.env(parent = baseenv())
  env$config <- config
  eval(quote(function(values) sum(values) + attr(config, "state")$offset), env)
}

test_that("attributes cannot hide mutable references in captured values", {
  state <- new.env(parent = emptyenv())
  state$offset <- 2
  state$self <- state
  for (config in list(
    structure(1, state = state),
    structure(list(1), state = list(nested = state)),
    structure(list(structure(1, state = state)), label = "outer")
  )) {
    callback <- reference_callback(config)
    expect_error(
      glyclean_pipeline(aggregate(f = callback)),
      "Mutable reference"
    )
  }
  x <- simple_exp(2, 2)
  before <- x
  expect_error(
    aggregate(
      x,
      standardize_variable = FALSE,
      f = reference_callback(structure(1, state = state))
    ),
    "Mutable reference"
  )
  expect_identical(x, before)
  expect_length(get_pipeline(x), 0L)
  callback <- function(values) sum(values)
  attr(callback, "state") <- state
  expect_error(glyclean_pipeline(aggregate(f = callback)), "Mutable reference")
})

test_that("S4 slots reject references but preserve ordinary value objects", {
  class_name <- "glyclean_freezing_test_config"
  methods::setClass(class_name, slots = c(value = "ANY"))
  on.exit(methods::removeClass(class_name), add = TRUE)
  state <- new.env(parent = emptyenv())
  state$self <- state
  for (payload in list(state, list(state), structure(1, state = state))) {
    object <- methods::new(class_name, value = payload)
    expect_error(.freeze_pipeline_value(object), "Mutable reference")
  }
  object <- methods::new(class_name, value = structure(c(2, 3), label = "keep"))
  expect_identical(.freeze_pipeline_value(object), object)
  expect_true(methods::validObject(.freeze_pipeline_value(object)))
})

test_that("closures inside attributes are frozen without sharing caller bindings", {
  env <- new.env(parent = baseenv())
  env$offset <- 2
  accessor <- eval(quote(function() offset), env)
  callback_env <- new.env(parent = baseenv())
  callback_env$config <- structure(1, accessor = accessor)
  callback <- eval(
    quote(function(values) sum(values) + attr(config, "accessor")()),
    callback_env
  )
  x <- simple_exp(2, 2)
  result <- aggregate(x, standardize_variable = FALSE, f = callback)
  history <- get_pipeline(result)
  env$offset <- 100
  replay <- apply_pipeline(x, history)
  expect_identical(
    SummarizedExperiment::assay(replay),
    SummarizedExperiment::assay(result)
  )
  expect_identical(history[[1]]$parameters$f(c(1, 2)), 5)
  restored <- unserialize(serialize(history, NULL))
  expect_identical(
    SummarizedExperiment::assay(apply_pipeline(x, restored)),
    SummarizedExperiment::assay(result)
  )
})

test_that("recursive closures fail promptly and ordinary attributed values survive", {
  env <- new.env(parent = baseenv())
  env$a <- eval(quote(function(x) b(x)), env)
  env$b <- eval(quote(function(x) a(x)), env)
  expect_error(.freeze_pipeline_value(env$a), "Recursive callback")
  for (value in list(
    factor(c("a", "b")),
    as.Date("2026-01-01"),
    structure(matrix(1:4, 2), label = list(code = "keep")),
    quote(x[, 1]),
    pairlist(a = 1),
    expression(x + 1)
  )) {
    expect_identical(.freeze_pipeline_value(value), value)
  }
  state <- new.env()
  for (value in list(
    pairlist(state = state),
    as.expression(list(state)),
    as.call(list(as.name("identity"), state))
  )) {
    expect_error(.freeze_pipeline_value(value), "Mutable reference")
  }
})
