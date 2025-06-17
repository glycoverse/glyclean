test_that("normalize_median works", {
  test_exp <- simple_exp(3, 3)
  result_exp <- normalize_median(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_median_abs works", {
  test_exp <- simple_exp(3, 3)
  result_exp <- normalize_median_abs(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_total_area works", {
  test_exp <- simple_exp(3, 3)
  result_exp <- normalize_total_area(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
  
  # all column sums should be 1
  expect_true(all(abs(colSums(result_exp$expr_mat) - 1) < 1e-10))
})


test_that("normalize_quantile works", {
  test_exp <- simple_exp(10, 10)
  result_exp <- normalize_quantile(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_loessf works", {
  test_exp <- simple_exp(10, 10)
  result_exp <- normalize_loessf(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_loesscyc works", {
  test_exp <- simple_exp(10, 10)
  result_exp <- normalize_loesscyc(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_vsn works", {
  test_exp <- simple_exp(50, 50)
  result_exp <- normalize_vsn(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_median_quotient works", {
  test_exp <- simple_exp(10, 10)
  result_exp <- normalize_median_quotient(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})





test_that("normalize_median_quotient with `by` specified works", {
  # Create a test experiment with grouping variable
  test_exp <- complex_exp()
  
  # Apply normalization grouped by "group"
  normed_exp <- normalize_median_quotient(test_exp, by = "group")
  
  expect_s3_class(normed_exp, "glyexp_experiment")
  expect_equal(dim(normed_exp$expr_mat), dim(test_exp$expr_mat))
})


test_that("normalize_rlr works", {
  test_exp <- simple_exp(10, 10)
  result_exp <- normalize_rlr(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_rlrma works", {
  test_exp <- simple_exp(10, 10)
  result_exp <- normalize_rlrma(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})


test_that("normalize_rlrmacyc works", {
  test_exp <- simple_exp(10, 10)
  result_exp <- normalize_rlrmacyc(test_exp)
  expect_snapshot(round(result_exp$expr_mat, 4))
})
