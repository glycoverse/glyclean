test_that("automatic removal preserves the actual filtered SE rows", {
  values <- rbind(
    c(1, 2, 3, 4, 5, 6),
    c(1, NA, NA, NA, NA, NA),
    c(NA, 2, 3, 4, 5, 6),
    rep(NA_real_, 6)
  )
  groups <- factor(rep(c("A", "B", "C"), each = 2), levels = c("A", "B", "C", "unused"))
  for (feature_names in list(NULL, rep("same", 4), paste0("V", 1:4))) {
    dimnames(values) <- list(feature_names, paste0("S", 1:6))
    exp <- SummarizedExperiment::SummarizedExperiment(
      assays = list(abundance = values),
      rowData = S4Vectors::DataFrame(original_position = 1:4),
      colData = S4Vectors::DataFrame(group = groups, original_position = 1:6),
      metadata = list(source = "position-fixture")
    )
    for (preset in c("simple", "discovery", "biomarker")) {
      result <- suppressMessages(auto_remove(exp, preset = preset))
      replay <- suppressMessages(apply_pipeline(exp, get_pipeline(result)))
      expected <- exp[c(1, 3), , drop = FALSE]

      expect_equal(SummarizedExperiment::assay(result), SummarizedExperiment::assay(expected))
      expect_equal(SummarizedExperiment::rowData(result), SummarizedExperiment::rowData(expected))
      expect_equal(SummarizedExperiment::colData(result), SummarizedExperiment::colData(exp))
      expect_equal(SummarizedExperiment::assay(replay), SummarizedExperiment::assay(expected))
      expect_identical(S4Vectors::metadata(result)$source, "position-fixture")
      expect_identical(SummarizedExperiment::assay(exp), values)
    }
  }
})

test_that("automatic removal preserves a named zero-row result", {
  mat <- matrix(NA_real_, nrow = 2, ncol = 3, dimnames = list(c("same", "same"), paste0("S", 1:3)))
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = mat),
    rowData = S4Vectors::DataFrame(original_position = 1:2)
  )
  result <- suppressMessages(auto_remove(exp, preset = "simple"))
  expect_equal(dim(result), c(0L, 3L))
  expect_identical(SummarizedExperiment::rowData(result)$original_position, integer())
})


test_that("rare filtering does not remove all rows when no groups are observed", {
  mat <- matrix(1, nrow = 2, ncol = 3)
  exp <- SummarizedExperiment::SummarizedExperiment(assays = list(abundance = mat))
  for (groups in list(rep(NA_character_, 3), factor(rep(NA_character_, 3), levels = "unused"))) {
    expect_error(
      remove_rare(exp, prop = 0.5, min_n = 1, by = groups),
      "at least one observed group"
    )
  }
})
