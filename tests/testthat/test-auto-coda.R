test_that("auto_coda rejects unsupported inputs", {
  expect_error(auto_coda(matrix(1:6, nrow = 2)), "glyexp_experiment")
})
