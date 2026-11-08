test_that("auto_coda rejects unsupported inputs", {
  expect_error(auto_coda(matrix(1:6, nrow = 2)), "glyexp_experiment")
})

test_that("auto_coda chooses legacy methods without deprecated dimension calls", {
  inputs <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    lapply(c(50L, 51L), \(n) {
      x <- simple_exp(n, 4)
      glyexp::experiment(
        .get_expr_mat(x),
        sample_info = legacy_sample_info(x),
        var_info = legacy_var_info(x),
        exp_type = "glycomics",
        glycan_type = "N"
      )
    })
  )
  withr::local_options(lifecycle_verbosity = "warning")
  called <- character()
  local_mocked_bindings(
    transform_clr = \(x, ...) {
      called <<- c(called, "clr")
      x
    },
    transform_alr = \(x, ...) {
      called <<- c(called, "alr")
      x
    }
  )
  for (input in inputs) {
    expect_warning(result <- suppressMessages(auto_coda(input)), NA)
    expect_identical(result, input)
  }
  expect_identical(called, c("clr", "alr"))
})
