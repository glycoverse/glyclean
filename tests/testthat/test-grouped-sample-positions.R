grouped_position_matrix <- function() {
  matrix(
    c(1, 10, 5, 100, 80, 90, 4, 6, 2, 30, 40, 50),
    nrow = 3,
    dimnames = list(paste0("V", 1:3), NULL)
  )
}

test_that("grouped processing restores exact positions independently of names", {
  values <- grouped_position_matrix()
  groups <- factor(c("B", "A", "B", "A"), levels = c("unused", "A", "B"))
  for (sample_names in list(NULL, c("same", "A", "same", "A"), paste0("S", 1:4))) {
    colnames(values) <- sample_names
    expect_identical(.apply_by_groups(values, identity, groups), values)
    expect_identical(.apply_by_groups(values, identity), values)

    center <- function(x) {
      expect_gt(ncol(x), 0)
      sweep(x, 1, rowMeans(x), "-")
    }
    expected <- values
    expected[, c(1, 3)] <- center(values[, c(1, 3), drop = FALSE])
    expected[, c(2, 4)] <- center(values[, c(2, 4), drop = FALSE])
    expect_equal(.apply_by_groups(values, center, groups), expected)
  }
})

test_that("grouped normalization keeps values aligned with sample metadata", {
  values <- grouped_position_matrix()
  groups <- factor(c("B", "A", "B", "A"), levels = c("unused", "A", "B"))
  expected <- values
  expected[, c(1, 3)] <- limma::normalizeQuantiles(values[, c(1, 3), drop = FALSE])
  expected[, c(2, 4)] <- limma::normalizeQuantiles(values[, c(2, 4), drop = FALSE])

  for (sample_names in list(NULL, c("same", "A", "same", "A"), paste0("S", 1:4))) {
    colnames(values) <- sample_names
    exp <- SummarizedExperiment::SummarizedExperiment(
      assays = list(abundance = values),
      colData = S4Vectors::DataFrame(batch = groups, original_position = 1:4)
    )
    result <- normalize_quantile(exp, by = "batch")
    replay <- apply_pipeline(exp, get_pipeline(result))

    expect_equal(unname(SummarizedExperiment::assay(result)), unname(expected))
    expect_equal(unname(SummarizedExperiment::assay(replay)), unname(expected))
    expect_identical(SummarizedExperiment::colData(result)$batch, groups)
    expect_identical(SummarizedExperiment::colData(result)$original_position, 1:4)
    if (!is.null(sample_names)) {
      expect_identical(colnames(result), sample_names)
      expect_identical(colnames(replay), sample_names)
    }
    expect_identical(SummarizedExperiment::assay(exp), values)
  }
})

test_that("missing group assignments cannot silently discard samples", {
  values <- grouped_position_matrix()
  expect_error(
    .apply_by_groups(values, identity, c("A", NA, "B", "A")),
    "Grouping values cannot contain missing values"
  )
})
