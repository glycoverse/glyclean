test_that("filters track positions with absent or duplicate row names", {
  mat <- rbind(c(1, 1, 1), c(2, 4, 6), c(NA, NA, NA))
  for (nms in list(NULL, c("duplicate", "duplicate", "third"))) {
    for (scale in c("raw", "log2")) {
      values <- if (scale == "raw") mat else log2(mat)
      rownames(values) <- nms
      x <- SummarizedExperiment::SummarizedExperiment(
        assays = list(signal = values),
        rowData = S4Vectors::DataFrame(
          id = c("constant", "varying", "missing")
        ),
        colData = S4Vectors::DataFrame(group = c("A", "A", "B")),
        metadata = list(scale = scale, custom = "keep")
      )
      for (case in list(
        list(fun = remove_constant, args = list(), keep = c(2L, 3L)),
        list(fun = remove_rare, args = list(), keep = 1:2),
        list(
          fun = remove_low_var,
          args = list(var_cutoff = 0),
          keep = c(2L, 3L)
        )
      )) {
        result <- do.call(case$fun, c(list(x), case$args))
        expected <- x[case$keep, , drop = FALSE]
        expect_identical(
          SummarizedExperiment::assay(result),
          SummarizedExperiment::assay(expected)
        )
        expect_identical(
          SummarizedExperiment::rowData(result),
          SummarizedExperiment::rowData(expected)
        )
        expect_identical(
          SummarizedExperiment::colData(result),
          SummarizedExperiment::colData(x)
        )
        expect_identical(rownames(result), rownames(expected))
        expect_identical(S4Vectors::metadata(result)$custom, "keep")
        expect_identical(apply_pipeline(x, get_pipeline(result)), result)
      }
      grouped <- remove_constant(x, by = "group", strict = TRUE)
      expect_identical(
        SummarizedExperiment::assay(grouped),
        values[c(2, 3), , drop = FALSE]
      )
    }
  }
})

test_that("filters preserve unnamed all-kept and zero-row results", {
  for (mat in list(
    matrix(c(1, 2, 3, 4), 2),
    matrix(1, 2, 3),
    matrix(numeric(), 0, 3)
  )) {
    x <- SummarizedExperiment::SummarizedExperiment(assays = list(signal = mat))
    keep <- if (length(mat) && all(mat == 1)) integer() else seq_len(nrow(mat))
    result <- remove_constant(x)
    expect_identical(
      SummarizedExperiment::assay(result),
      mat[keep, , drop = FALSE]
    )
    expect_identical(apply_pipeline(x, get_pipeline(result)), result)
  }
})
