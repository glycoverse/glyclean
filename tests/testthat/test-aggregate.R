test_that("aggregating to glycopeptides works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gp", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c(
      "peptide",
      "protein",
      "gene",
      "glycan_composition",
      "peptide_site",
      "protein_site"
    )
  )
})

test_that("aggregating to glycoforms works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c("protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("default aggregation level depends on experiment type", {
  expect_null(formals(aggregate)$to_level)

  glycomics_without_structure <- aggregation_glycomic_se(
    include_structure = FALSE
  )
  glycoproteomics_without_structure <- complex_exp()
  SummarizedExperiment::rowData(
    glycoproteomics_without_structure
  )$glycan_structure <- NULL
  cases <- list(
    list(exp = aggregation_glycomic_se(), level = "gs"),
    list(exp = glycomics_without_structure, level = "g"),
    list(exp = complex_exp(), level = "gfs"),
    list(exp = glycoproteomics_without_structure, level = "gf")
  )

  for (case in cases) {
    result <- aggregate(case$exp, standardize_variable = FALSE)
    expected <- aggregate(
      case$exp,
      to_level = case$level,
      standardize_variable = FALSE
    )
    expect_equal(
      SummarizedExperiment::assay(result),
      SummarizedExperiment::assay(expected)
    )
    expect_equal(
      SummarizedExperiment::rowData(result),
      SummarizedExperiment::rowData(expected)
    )
  }
})

test_that("aggregation preserves group order, missing-value sums, and metadata", {
  exp <- complex_exp()
  input_mat <- SummarizedExperiment::assay(exp)
  first_group <- c(1, 2, 4, 5, 6, 7)
  input_mat[first_group, 1] <- NA_real_
  input_mat[1, 2] <- NA_real_
  SummarizedExperiment::assay(exp) <- input_mat

  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  result_mat <- SummarizedExperiment::assay(res)
  result_var_info <- SummarizedExperiment::rowData(res)
  expected_mat <- rbind(
    colSums(input_mat[first_group, , drop = FALSE], na.rm = TRUE),
    input_mat[3, ],
    input_mat[8, ]
  )
  rownames(expected_mat) <- paste0("V", seq_len(nrow(expected_mat)))

  expect_equal(result_mat, expected_mat)
  expect_equal(result_var_info$protein_site, c(24L, 25L, 24L))
  expected_compositions <- SummarizedExperiment::rowData(
    exp
  )$glycan_composition[
    c(1, 3, 8)
  ]
  expect_equal(
    as.character(result_var_info$glycan_composition),
    as.character(expected_compositions)
  )
  expect_true(
    glyrepr::is_glycan_composition(result_var_info$glycan_composition)
  )
  expect_true("gene" %in% colnames(result_var_info))
  expect_false("peptide" %in% colnames(result_var_info))
  expect_false("charge" %in% colnames(result_var_info))
})

test_that("glycomics aggregation supports composition and structure levels", {
  exp <- aggregation_glycomic_se()

  compositions <- aggregate(exp, to_level = "g", standardize_variable = FALSE)
  structures <- aggregate(exp, to_level = "gs", standardize_variable = FALSE)

  expect_s4_class(compositions, "GlycomicSE")
  expect_equal(
    SummarizedExperiment::assay(compositions),
    matrix(
      c(6, 4, 18, 8),
      nrow = 2,
      dimnames = list(c("V1", "V2"), c("S1", "S2"))
    )
  )
  expect_setequal(
    colnames(SummarizedExperiment::rowData(compositions)),
    "glycan_composition"
  )

  expect_s4_class(structures, "GlycomicSE")
  expect_equal(
    SummarizedExperiment::assay(structures),
    matrix(
      c(3, 3, 4, 11, 7, 8),
      nrow = 3,
      dimnames = list(paste0("V", 1:3), c("S1", "S2"))
    )
  )
  expect_setequal(
    colnames(SummarizedExperiment::rowData(structures)),
    c("glycan_composition", "glycan_structure", "source")
  )
})

test_that("glycomics aggregation supports legacy experiments", {
  se <- aggregation_glycomic_se()
  exp <- suppressWarnings(
    glyexp::experiment(
      SummarizedExperiment::assay(se),
      sample_info = tibble::as_tibble(
        SummarizedExperiment::colData(se),
        rownames = "sample"
      ),
      var_info = tibble::as_tibble(
        SummarizedExperiment::rowData(se),
        rownames = "variable"
      ),
      exp_type = "glycomics",
      glycan_type = "N"
    )
  )

  result <- aggregate(exp, standardize_variable = FALSE)

  expect_s3_class(result, "glyexp_experiment")
  expect_identical(result$meta_data$exp_type, "glycomics")
  expect_equal(nrow(result$expr_mat), 3L)
})

test_that("aggregation levels must match the experiment type", {
  expect_snapshot(
    aggregate(
      aggregation_glycomic_se(),
      to_level = "gf",
      standardize_variable = FALSE
    ),
    error = TRUE
  )
  expect_snapshot(
    aggregate(
      complex_exp(),
      to_level = "g",
      standardize_variable = FALSE
    ),
    error = TRUE
  )
})

test_that("glycomics structure aggregation requires glycan structures", {
  expect_snapshot(
    aggregate(
      aggregation_glycomic_se(include_structure = FALSE),
      to_level = "gs",
      standardize_variable = FALSE
    ),
    error = TRUE
  )
})

test_that("aggregating to glycopeptides (with structures) works", {
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gps", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c(
      "peptide",
      "protein",
      "gene",
      "glycan_composition",
      "glycan_structure",
      "peptide_site",
      "protein_site"
    )
  )
})

test_that("aggregating to glycoforms (with structures) works", {
  skip("Cannot be easily tested with currenct settings.")
  # The actual result has "peptide" and "peptide_site" columns.
  # This is because these two columns have "many-to-one" relationship with the aggregation columns.
  # That is, one glycoform (with structures) happens to have only one peptide and one peptide site
  # in our test dataset.
  exp <- real_exp()
  res <- aggregate(exp, to_level = "gfs", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c(
      "protein",
      "gene",
      "glycan_composition",
      "glycan_structure",
      "protein_site"
    )
  )
})

test_that("aggregating from glycopeptides to glycoforms works", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gp", standardize_variable = FALSE)
  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c("protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregating from glycoforms to glycopeptides fails", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_snapshot(
    aggregate(exp, to_level = "gp", standardize_variable = FALSE),
    error = TRUE
  )
})


test_that("aggregating from glycoforms with structure to glycoforms without structures works", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gfs", standardize_variable = FALSE)
  res <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_setequal(
    colnames(SummarizedExperiment::rowData(res)),
    c("protein", "gene", "glycan_composition", "protein_site")
  )
})

test_that("aggregating from glycoforms without structures to glycoforms with structures fails", {
  exp <- real_exp()
  exp <- aggregate(exp, to_level = "gf", standardize_variable = FALSE)
  expect_snapshot(
    aggregate(exp, to_level = "gfs", standardize_variable = FALSE),
    error = TRUE
  )
})

test_that("custom aggregation works across supported containers", {
  se <- aggregation_glycomic_se()
  legacy <- suppressWarnings(glyexp::from_se(
    se,
    exp_type = "glycomics",
    glycan_type = "N"
  ))
  for (exp in list(se, legacy, complex_exp())) {
    level <- if (inherits(exp, "GlycoproteomicSE")) "gf" else "g"
    input <- .get_expr_mat(exp)
    groups <- if (level == "gf") {
      list(c(1, 2, 4, 5, 6, 7), 3, 8)
    } else {
      list(1:3, 4)
    }
    for (f in list(mean, max, function(x) sum(x) / 2)) {
      result <- aggregate(exp, level, FALSE, f = f)
      expected <- t(vapply(
        groups,
        function(rows) {
          vapply(
            seq_len(ncol(input)),
            function(j) f(input[rows, j]),
            numeric(1)
          )
        },
        numeric(ncol(input))
      ))
      dimnames(expected) <- list(
        paste0("V", seq_along(groups)),
        colnames(input)
      )
      expect_equal(.get_expr_mat(result), expected)
      expect_identical(class(result), class(exp))
      expect_equal(.get_sample_info(result), .get_sample_info(exp))
      expect_equal(
        .get_var_info(result),
        .get_var_info(aggregate(exp, level, FALSE))
      )
    }
  }
})

test_that("explicit sum preserves defaults and custom functions receive no missing values", {
  exp <- aggregation_glycomic_se()[, 1, drop = FALSE]
  input <- SummarizedExperiment::assay(exp)
  input[1:3, 1] <- NA_real_
  SummarizedExperiment::assay(exp) <- input
  default <- aggregate(exp, "g", FALSE)
  expect_identical(default, aggregate(exp, "g", FALSE, f = sum))
  expect_equal(as.numeric(SummarizedExperiment::assay(default)), c(0, 4))
  result <- aggregate(exp, "g", FALSE, f = function(x) {
    expect_identical(anyNA(x), FALSE)
    length(x)
  })
  expect_equal(
    SummarizedExperiment::assay(result),
    matrix(
      c(0, 1),
      ncol = 1,
      dimnames = list(c("V1", "V2"), "S1")
    )
  )
  result <- aggregate(exp[4, , drop = FALSE], "g", FALSE, f = max)
  expect_equal(dim(SummarizedExperiment::assay(result)), c(1L, 1L))
  expect_equal(as.numeric(SummarizedExperiment::assay(result)), 4)
})

test_that("aggregation validates the function and its result", {
  exp <- aggregation_glycomic_se()
  expect_snapshot(aggregate(exp, "g", FALSE, f = "sum"), error = TRUE)
  expect_snapshot(aggregate(exp, "g", FALSE, f = identity), error = TRUE)
  expect_snapshot(
    aggregate(exp, "g", FALSE, f = function(x) "bad"),
    error = TRUE
  )
})
