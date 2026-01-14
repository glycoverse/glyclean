test_that("add_site_seq works correctly", {
  # Create a toy experiment with protein and protein_site columns
  expr_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  colnames(expr_mat) <- c("S1", "S2", "S3")
  rownames(expr_mat) <- c("V1", "V2")

  sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"),
    group = c("A", "B", "A")
  )

  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    protein = c("P12345", "Q67890"),
    protein_site = c(10L, 5L),
    glycan_composition = c("H5N2", "H3N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  # Create a temporary FASTA file using withr
  fasta_content <- c(
    ">sp|P12345|TEST1_HUMAN Test protein 1",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    ">sp|Q67890|TEST2_HUMAN Test protein 2",
    "MNPQRSTUVW"
  )

  withr::with_tempfile("temp_fasta", fileext = ".fasta", {
    writeLines(fasta_content, temp_fasta)

    # Test the function
    result <- suppressMessages(add_site_seq(exp, temp_fasta, n_aa = 3))

    # Check that site_sequence column was added
    expect_true("site_sequence" %in% colnames(result$var_info))

    # Check the sequences
    # P12345 site 10 (J) with n_aa=3: GHIJKLM
    expect_equal(result$var_info$site_sequence[1], "GHIJKLM")

    # Q67890 site 5 (R) with n_aa=3: NPQRSTU (R is at position 5, sequence is MNPQRSTUVW)
    expect_equal(result$var_info$site_sequence[2], "NPQRSTU")
  })
})

test_that("add_site_seq handles missing proteins gracefully", {
  # Create experiment with protein not in FASTA
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- c("S1", "S2")
  rownames(expr_mat) <- c("V1")

  sample_info <- tibble::tibble(
    sample = c("S1", "S2"),
    group = c("A", "B")
  )

  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("MISSING"),
    protein_site = c(10L),
    glycan_composition = c("H5N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  # Create a temporary FASTA file without the missing protein using withr
  fasta_content <- c(
    ">sp|P12345|TEST1_HUMAN Test protein 1",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  )

  withr::with_tempfile("temp_fasta", fileext = ".fasta", {
    writeLines(fasta_content, temp_fasta)

    # Test with missing protein (should use X without warning)
    suppressMessages(result <- add_site_seq(exp, temp_fasta, n_aa = 3))

    # Should return NA
    expect_true(is.na(result$var_info$site_sequence[1]))
  })
})

test_that("add_site_seq handles out-of-range sites gracefully", {
  # Create experiment with site out of range
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- c("S1", "S2")
  rownames(expr_mat) <- c("V1")

  sample_info <- tibble::tibble(
    sample = c("S1", "S2"),
    group = c("A", "B")
  )

  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("P12345"),
    protein_site = c(100L),  # Out of range
    glycan_composition = c("H5N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  # Create a temporary FASTA file using withr
  fasta_content <- c(
    ">sp|P12345|TEST1_HUMAN Test protein 1",
    "ABCDEFGHIJ"  # Only 10 amino acids
  )

  withr::with_tempfile("temp_fasta", fileext = ".fasta", {
    writeLines(fasta_content, temp_fasta)

    # Test with out-of-range site (should use X without warning)
    suppressMessages(result <- add_site_seq(exp, temp_fasta, n_aa = 3))

    # Should return all X's
    expect_true(is.na(result$var_info$site_sequence[1]))
  })
})

test_that("add_site_seq rejects matrix input", {
  mat <- matrix(1:6, nrow = 2)

  withr::with_tempfile("temp_fasta", fileext = ".fasta", {
    writeLines(">P12345\nABCDEFGHIJ", temp_fasta)

    expect_error(
      add_site_seq(mat, temp_fasta),
      "must be a <glyexp_experiment> object"
    )
  })
})

test_that(".validate_protein_seqs normalizes sequences", {
  # Test uppercase conversion
  seqs <- c(P12345 = "abcxyz", Q67890 = "mnoabc")
  result <- .validate_protein_seqs(seqs)
  expect_equal(unname(result["P12345"]), "ABCXYZ")
  expect_equal(unname(result["Q67890"]), "MNOABC")

  # Test whitespace removal
  seqs <- c(P12345 = "A B C X Y Z")
  result <- .validate_protein_seqs(seqs)
  expect_equal(unname(result["P12345"]), "ABCXYZ")
})

test_that(".validate_protein_seqs errors on empty sequences", {
  seqs <- c(P12345 = "ABC", P67890 = "")
  expect_error(.validate_protein_seqs(seqs), "non-empty")
})

test_that("add_site_seq accepts named character vector for fasta", {
  # Create experiment
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- c("S1", "S2")
  rownames(expr_mat) <- c("V1")

  sample_info <- tibble::tibble(
    sample = c("S1", "S2"),
    group = c("A", "B")
  )

  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("P12345"),
    protein_site = c(10L),
    glycan_composition = c("H5N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  # Test with named character vector
  fasta_vec <- c(P12345 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  result <- suppressMessages(add_site_seq(exp, fasta_vec, n_aa = 3))

  expect_true("site_sequence" %in% colnames(result$var_info))
  expect_equal(result$var_info$site_sequence[1], "GHIJKLM")
})

test_that("add_site_seq rejects unnamed character vector for fasta", {
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- c("S1", "S2")
  rownames(expr_mat) <- c("V1")

  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("P12345"),
    protein_site = c(10L),
    glycan_composition = c("H5N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  # Test with unnamed character vector
  expect_error(
    add_site_seq(exp, c("ABCDEFGHIJKLMNOPQRSTUVWXYZ")),
    "`fasta`"
  )
})

test_that("add_site_seq shows correct message for character vector input", {
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- c("S1", "S2")
  rownames(expr_mat) <- c("V1")

  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("P12345"),
    protein_site = c(10L),
    glycan_composition = c("H5N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  fasta_vec <- c(P12345 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  expect_message(
    add_site_seq(exp, fasta_vec, n_aa = 3),
    '"Provided" contains 1 protein sequences'
  )
})

test_that("add_site_seq fetches from UniProt when fasta is NULL", {
  skip_if_not_installed("mockr")

  # Create a simple experiment
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- c("S1", "S2")
  rownames(expr_mat) <- c("V1")

  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("P12345"),
    protein_site = c(10L),
    glycan_composition = c("H5N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  # Mock .fetch_uniprot_sequences to return a test sequence
  mock_seq <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  mockr::local_mock(
    `.fetch_uniprot_sequences` = function(proteins, taxid = 9606) {
      stats::setNames(mock_seq, proteins)
    }
  )

  # Test with fasta = NULL (should fetch from UniProt)
  suppressMessages(result <- add_site_seq(exp, fasta = NULL, n_aa = 3))

  expect_true("site_sequence" %in% colnames(result$var_info))
  # Site 10 with n_aa=3: positions 7-13 = GHIJKLM
  expect_equal(result$var_info$site_sequence[1], "GHIJKLM")
})

test_that("add_site_seq uses custom taxid", {
  skip_if_not_installed("mockr")

  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- c("S1", "S2")
  rownames(expr_mat) <- c("V1")

  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("P12345"),
    protein_site = c(10L),
    glycan_composition = c("H5N2")
  )

  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )

  mock_seq <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  captured_taxid <- NULL

  mockr::local_mock(
    `.fetch_uniprot_sequences` = function(proteins, taxid = 9606) {
      captured_taxid <<- taxid
      stats::setNames(mock_seq, proteins)
    }
  )

  suppressMessages(result <- add_site_seq(exp, fasta = NULL, n_aa = 3, taxid = 10090))

  expect_equal(captured_taxid, 10090)
  expect_true("site_sequence" %in% colnames(result$var_info))
})

test_that(".fetch_uniprot_sequences merges batches without prefixing names", {
  skip_if_not_installed("mockr")

  mockr::local_mock(
    `.query_uniprot` = function(query, fields = c("accession", "sequence"), ...) {
      protein <- stringr::str_match(query, "accession:([A-Z0-9]+)")[, 2]
      data.frame(
        Entry = protein,
        Sequence = stringr::str_c("SEQ_", protein),
        stringsAsFactors = FALSE
      )
    },
    .env = rlang::ns_env("glyclean")
  )

  suppressMessages(seqs <- .fetch_uniprot_sequences(c("P1", "P2"), batch_size = 1))

  expect_equal(names(seqs), c("P1", "P2"))
  expect_equal(unname(seqs), c("SEQ_P1", "SEQ_P2"))
})

test_that(".uniprot_result_to_seqs uses Entry column", {
  result <- data.frame(
    Entry = c("P12345", "Q67890"),
    Sequence = c("AAA", "BBB"),
    stringsAsFactors = FALSE
  )

  seqs <- .uniprot_result_to_seqs(result)
  expect_equal(seqs, c(P12345 = "AAA", Q67890 = "BBB"))
})

test_that(".uniprot_result_to_seqs falls back to Accession column", {
  result <- data.frame(
    Accession = c("P12345"),
    Sequence = c("AAA"),
    stringsAsFactors = FALSE
  )

  seqs <- .uniprot_result_to_seqs(result)
  expect_equal(seqs, c(P12345 = "AAA"))
})

test_that(".uniprot_result_to_seqs errors when Sequence column is missing", {
  result <- data.frame(Entry = c("P12345"), stringsAsFactors = FALSE)
  expect_error(.uniprot_result_to_seqs(result), "Sequence")
})
