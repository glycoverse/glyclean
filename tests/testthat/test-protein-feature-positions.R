protein_position_fixture <- function(feature_names) {
  mat <- rbind(c(2, 5, 9, 8), c(8, 6, 4, 3), c(9, 4, 3, 5))
  dimnames(mat) <- list(feature_names, paste0("S", 1:4))
  glyexp::GlycoproteomicSE(
    mat,
    rowData = S4Vectors::DataFrame(
      protein = c("P1", "P2", "unmatched"),
      protein_site = c(10L, 20L, 30L),
      glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2)), 3),
      original_position = 1:3
    ),
    colData = S4Vectors::DataFrame(original_position = 1:4),
    metadata = list(glycan_type = "N")
  )
}

test_that("protein adjustment aligns feature annotations independently of names", {
  proteins <- rbind(P2 = c(3, 5, 9, 6), P1 = c(8, 2, 4, 7))
  colnames(proteins) <- c("S4", "S2", "S1", "other")
  for (feature_names in list(NULL, c("same", "same", "same"), paste0("V", 1:3))) {
    exp <- protein_position_fixture(feature_names)
    mat <- SummarizedExperiment::assay(exp)
    retained <- c(1L, 2L, 4L)
    gp <- mat[1:2, retained, drop = FALSE]
    pro <- proteins[c("P1", "P2"), c("S1", "S2", "S4"), drop = FALSE]
    expected_ratio <- t(vapply(1:2, function(i) {
      (gp[i, ] / pro[i, ]) / (stats::median(gp[i, ]) / stats::median(pro[i, ]))
    }, numeric(3)))
    expected_reg <- t(vapply(1:2, function(i) {
      2^stats::residuals(stats::lm(log2(gp[i, ] + 1) ~ log2(pro[i, ] + 1)))
    }, numeric(3)))

    for (method in c("ratio", "reg")) {
      result <- suppressMessages(adjust_protein(exp, proteins, method = method))
      expected <- if (method == "ratio") expected_ratio else expected_reg
      expect_equal(unname(SummarizedExperiment::assay(result)), unname(expected))
      expect_identical(rownames(result), if (is.null(feature_names)) NULL else feature_names[1:2])
      expect_identical(SummarizedExperiment::rowData(result)$protein, c("P1", "P2"))
      expect_identical(SummarizedExperiment::rowData(result)$original_position, 1:2)
      expect_identical(SummarizedExperiment::colData(result)$original_position, retained)
      expect_identical(SummarizedExperiment::assay(exp), mat)
    }
  }
})

test_that("internal SE subsetting keeps repeated indices and annotations aligned", {
  for (feature_names in list(NULL, c("same", "same", "same"))) {
    exp <- protein_position_fixture(feature_names)
    expect_equal(.subset_container(exp, rows = c(2, 1, 2), cols = c(4, 2)), exp[c(2, 1, 2), c(4, 2)])
  }
})
