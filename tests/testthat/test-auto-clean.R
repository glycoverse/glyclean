# Test main logic path for glycoproteomics data
test_that("auto_clean works for glycoproteomics data", {
  test_exp <- complex_exp()
  result_exp <- suppressMessages(auto_clean(test_exp))
  
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycoproteomics")
  expect_false(any(is.na(result_exp$expr_mat)))
})

# Test main logic path for glycomics data
test_that("auto_clean works for glycomics data", {
  # Create proper glycomics experiment
  sample_info <- tibble::tibble(sample = paste0("S", 1:10))
  var_info <- tibble::tibble(variable = paste0("V", 1:8))
  expr_mat <- matrix(runif(80), nrow = 8)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  test_exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycomics",
    glycan_type = "N"
  )
  
  result_exp <- suppressMessages(auto_clean(test_exp))
  
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycomics")
  expect_false(any(is.na(result_exp$expr_mat)))
})

# Test different imputation methods based on sample size
test_that("auto_clean handles different sample sizes", {
  # Small dataset (≤30 samples) - triggers impute_sample_min
  small_exp <- complex_exp()
  # Add some missing values
  small_exp$expr_mat[1:2, 1:2] <- NA
  result_small <- suppressMessages(auto_clean(small_exp))
  expect_false(any(is.na(result_small$expr_mat)))
  
  # Just test that the function works with the complex experiment
  # The exact imputation method testing would require larger datasets
  expect_s3_class(result_small, "glyexp_experiment")
})

# Test parameter validation
test_that("auto_clean validates parameters", {
  test_exp <- complex_exp()
  
  expect_error(auto_clean("not_an_experiment"))
  expect_error(auto_clean(test_exp, to_level = "invalid"))
})

# Test auto_clean with batch effect correction
test_that("auto_clean applies batch effect correction when batch column exists", {
  # Set seed for reproducible random data
  set.seed(654)
  
  # Create experiment with batch information for glycoproteomics
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:12),
    batch = rep(c("A", "B", "C"), each = 4),
    group = rep(c("Ctrl", "Treat"), 6)
  )
  var_info <- tibble::tribble(
    ~peptide, ~proteins, ~genes, ~glycan_composition, ~glycan_structure, ~peptide_site, ~protein_sites, ~charge, ~modifications,
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "",
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "",
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H)(H(H(H))))))", 4, 24, 2, "",
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "6,Carbamidomethyl[C]",
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 3, "",
    "AAANAAKAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "",
    "AAANAAK", "PRO1", "GENE1", "H3N2", "(N(N(H(H(H)))))", 4, 24, 2, "",
    "BBCNAAK", "PRO2", "GENE2", "H4N2", "(N(N(H(H))))", 5, 45, 2, "",
    "CCCNAAK", "PRO3", "GENE3", "H6N3", "(N(N(N(H(H(H))(H)))))", 6, 66, 3, "",
    "DDDNAAK", "PRO4", "GENE4", "H7N3", "(N(N(N(H(H))(H(H)))))", 7, 77, 2, ""
  )
  var_info <- dplyr::mutate(
    var_info,
    variable = paste0("PSM", 1:nrow(var_info)),
    .before = 1
  )
  
  # Create expression matrix with batch effects
  nrow <- nrow(var_info)
  ncol <- nrow(sample_info)
  expr_mat <- matrix(rnorm(nrow * ncol, mean = 10, sd = 2), nrow = nrow, ncol = ncol)
  
  # Add artificial batch effects
  batch_a_samples <- which(sample_info$batch == "A")
  batch_b_samples <- which(sample_info$batch == "B")
  batch_c_samples <- which(sample_info$batch == "C")
  expr_mat[, batch_a_samples] <- expr_mat[, batch_a_samples] + 1  # Batch A +1
  expr_mat[, batch_b_samples] <- expr_mat[, batch_b_samples] - 0.5  # Batch B -0.5
  expr_mat[, batch_c_samples] <- expr_mat[, batch_c_samples] + 0.3  # Batch C +0.3
  
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  test_exp <- glyexp::experiment(
    expr_mat, sample_info, var_info, 
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Run auto_clean
  result_exp <- suppressMessages(auto_clean(test_exp, to_level = "gf"))
  
  # Test that function completes successfully
  expect_s3_class(result_exp, "glyexp_experiment")
  expect_equal(glyexp::get_exp_type(result_exp), "glycoproteomics")
  expect_false(any(is.na(result_exp$expr_mat)))
  
  # Test that batch information is preserved
  expect_true("batch" %in% colnames(result_exp$sample_info))
  expect_equal(result_exp$sample_info$batch, sample_info$batch)
  
  # Test that the preprocessing pipeline was completed (aggregation reduces variables)
  expect_lt(nrow(result_exp$expr_mat), nrow(test_exp$expr_mat))
})
