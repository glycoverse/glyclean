test_that("adjust_protein works with ratio method", {
  # Create test glycoproteomics experiment
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:4),
    group = c("A", "A", "B", "B")
  )
  
  var_info <- tibble::tibble(
    variable = paste0("GP", 1:4),
    protein = c("PRO1", "PRO2", "PRO1", "PRO3"),
    protein_site = c(1L, 2L, 3L, 4L),
    peptide = c("AAANAAK", "BBBNAAB", "CCCNAAC", "DDDNAAD"),
    glycan_composition = c("H5N2", "H3N2", "H4N2", "H5N3")
  )
  
  # Create expression matrix with known values
  expr_mat <- matrix(
    c(100, 200, 300, 400,  # GP1: PRO1
      50, 100, 150, 200,   # GP2: PRO2
      200, 400, 600, 800,  # GP3: PRO1
      80, 160, 240, 320),  # GP4: PRO3
    nrow = 4, ncol = 4, byrow = TRUE
  )
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )
  
  # Create protein expression matrix
  pro_expr_mat <- matrix(
    c(10, 20, 30, 40,  # PRO1
      5, 10, 15, 20,   # PRO2
      4, 8, 12, 16),   # PRO3
    nrow = 3, ncol = 4, byrow = TRUE
  )
  colnames(pro_expr_mat) <- sample_info$sample
  rownames(pro_expr_mat) <- c("PRO1", "PRO2", "PRO3")
  
  # Test ratio method
  result <- adjust_protein(exp, pro_expr_mat, method = "ratio")
  
  # Check that result is still an experiment
  expect_s3_class(result, "glyexp_experiment")
  
  # Check dimensions
  expect_equal(nrow(result$expr_mat), 4)
  expect_equal(ncol(result$expr_mat), 4)
  
  # Check sample info is preserved
  expect_equal(result$sample_info, exp$sample_info)
  
  # Check var info is preserved
  expect_equal(result$var_info, exp$var_info)
  
  # Check that adjusted values are reasonable
  # For GP1 (PRO1): ratio should be (100,200,300,400) / (10,20,30,40) = (10,10,10,10)
  # Scaling factor should be median(GP1) / median(PRO1) = 250 / 25 = 10
  # Adjusted: (10,10,10,10) / 10 = (1,1,1,1)
  expect_equal(as.numeric(result$expr_mat["GP1", ]), c(1, 1, 1, 1))
  
  # For GP2 (PRO2): ratio should be (50,100,150,200) / (5,10,15,20) = (10,10,10,10)
  # Scaling factor should be median(GP2) / median(PRO2) = 125 / 12.5 = 10
  # Adjusted: (10,10,10,10) / 10 = (1,1,1,1)
  expect_equal(as.numeric(result$expr_mat["GP2", ]), c(1, 1, 1, 1))
})

test_that("adjust_protein works with reg method", {
  # Create test data
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:6)
  )
  
  var_info <- tibble::tibble(
    variable = paste0("GP", 1:2),
    protein = c("PRO1", "PRO2"),
    protein_site = c(1L, 2L),
    peptide = c("AAANAAK", "BBBNAAB"),
    glycan_composition = c("H5N2", "H3N2")
  )
  
  # Create expression matrix where GP = 2 * PRO + 5 + noise
  set.seed(123)
  pro_values1 <- c(1, 2, 3, 4, 5, 6)
  pro_values2 <- c(2, 4, 6, 8, 10, 12)
  
  expr_mat <- matrix(
    c(2 * pro_values1 + 5 + rnorm(6, 0, 0.1),  # GP1: 2*PRO1 + 5 + noise
      2 * pro_values2 + 5 + rnorm(6, 0, 0.1)),  # GP2: 2*PRO2 + 5 + noise
    nrow = 2, ncol = 6, byrow = TRUE
  )
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )
  
  # Create protein expression matrix
  pro_expr_mat <- matrix(
    c(pro_values1,  # PRO1
      pro_values2), # PRO2
    nrow = 2, ncol = 6, byrow = TRUE
  )
  colnames(pro_expr_mat) <- sample_info$sample
  rownames(pro_expr_mat) <- c("PRO1", "PRO2")
  
  # Test regression method
  result <- adjust_protein(exp, pro_expr_mat, method = "reg")
  
  # Check that result is still an experiment
  expect_s3_class(result, "glyexp_experiment")
  
  # Check dimensions
  expect_equal(nrow(result$expr_mat), 2)
  expect_equal(ncol(result$expr_mat), 6)
  
  # Check that adjusted values are close to 1 (since residuals should be close to 0, and 2^0 = 1)
  expect_true(all(abs(result$expr_mat["GP1", ] - 1) < 0.5))  # Should be close to 1
  expect_true(all(abs(result$expr_mat["GP2", ] - 1) < 0.5))  # Should be close to 1
  
  # Check that all adjusted values are positive (using 2^residuals ensures this)
  expect_true(all(result$expr_mat > 0, na.rm = TRUE))
})

test_that("adjust_protein handles missing protein column", {
  exp <- simple_exp(3, 4)
  exp$meta_data$exp_type <- "glycoproteomics"
  pro_expr_mat <- matrix(1:12, nrow = 3, ncol = 4)
  colnames(pro_expr_mat) <- paste0("S", 1:4)
  rownames(pro_expr_mat) <- c("PRO1", "PRO2", "PRO3")
  
  expect_error(
    adjust_protein(exp, pro_expr_mat, method = "ratio"),
    "protein.*column does not exist"
  )
})

test_that("adjust_protein handles no common proteins", {
  exp <- simple_exp(3, 4)
  exp$meta_data$exp_type <- "glycoproteomics"
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  
  pro_expr_mat <- matrix(1:12, nrow = 3, ncol = 4)
  rownames(pro_expr_mat) <- c("PRO4", "PRO5", "PRO6")  # Different proteins
  colnames(pro_expr_mat) <- paste0("S", 1:4)
  
  expect_error(
    adjust_protein(exp, pro_expr_mat, method = "ratio"),
    "No common proteins found"
  )
})

test_that("adjust_protein handles no common samples", {
  exp <- simple_exp(3, 4)
  exp$meta_data$exp_type <- "glycoproteomics"
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  
  pro_expr_mat <- matrix(1:12, nrow = 3, ncol = 4)
  rownames(pro_expr_mat) <- c("PRO1", "PRO2", "PRO3")
  colnames(pro_expr_mat) <- paste0("T", 1:4)  # Different sample names
  
  expect_error(
    adjust_protein(exp, pro_expr_mat, method = "ratio"),
    "No common samples found"
  )
})

test_that("adjust_protein handles partial sample overlap", {
  # Create test data with partial sample overlap
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:4),
    group = c("A", "A", "B", "B")
  )
  
  var_info <- tibble::tibble(
    variable = paste0("GP", 1:2),
    protein = c("PRO1", "PRO2"),
    protein_site = c(1L, 2L),
    peptide = c("AAANAAK", "BBBNAAB"),
    glycan_composition = c("H5N2", "H3N2")
  )
  
  expr_mat <- matrix(1:8, nrow = 2, ncol = 4)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )
  
  # Create protein expression matrix with only 3 samples (S1, S2, S3)
  pro_expr_mat <- matrix(1:6, nrow = 2, ncol = 3)
  colnames(pro_expr_mat) <- paste0("S", 1:3)
  rownames(pro_expr_mat) <- c("PRO1", "PRO2")
  
  result <- adjust_protein(exp, pro_expr_mat, method = "ratio")
  
  # Should only keep common samples (S1, S2, S3)
  expect_equal(ncol(result$expr_mat), 3)
  expect_equal(colnames(result$expr_mat), paste0("S", 1:3))
  expect_equal(nrow(result$sample_info), 3)
  expect_equal(result$sample_info$sample, paste0("S", 1:3))
})

test_that("adjust_protein handles regression with insufficient data", {
  # Create test data with only 2 samples
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:2)
  )
  
  var_info <- tibble::tibble(
    variable = "GP1",
    protein = "PRO1",
    peptide = "AAANAAK",
    protein_site = 1L,
    glycan_composition = "H5N2"
  )
  
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )
  
  pro_expr_mat <- matrix(c(10, 20), nrow = 1, ncol = 2)
  colnames(pro_expr_mat) <- sample_info$sample
  rownames(pro_expr_mat) <- "PRO1"
  
  # Should warn about insufficient samples and keep original values
  expect_warning(
    result <- adjust_protein(exp, pro_expr_mat, method = "reg"),
    "fewer than 3 valid samples"
  )
  
  # Original values should be preserved
  expect_equal(result$expr_mat, exp$expr_mat)
})

test_that("adjust_protein handles NA values correctly", {
  # Create test data with NA values
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:5)
  )
  
  var_info <- tibble::tibble(
    variable = "GP1",
    protein = "PRO1",
    protein_site = 1L,
    peptide = "AAANAAK", 
    glycan_composition = "H5N2"
  )
  
  expr_mat <- matrix(c(1, 2, NA, 4, 5), nrow = 1, ncol = 5)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )
  
  pro_expr_mat <- matrix(c(10, 20, 30, NA, 50), nrow = 1, ncol = 5)
  colnames(pro_expr_mat) <- sample_info$sample
  rownames(pro_expr_mat) <- "PRO1"
  
  # Test ratio method - should handle NA values gracefully
  result_ratio <- adjust_protein(exp, pro_expr_mat, method = "ratio")
  expect_true(is.na(result_ratio$expr_mat[1, 3]))  # GP has NA
  expect_true(is.na(result_ratio$expr_mat[1, 4]))  # PRO has NA
  
  # Test regression method - should only use valid samples
  result_reg <- adjust_protein(exp, pro_expr_mat, method = "reg")
  expect_true(is.na(result_reg$expr_mat[1, 3]))  # GP has NA
  expect_true(is.na(result_reg$expr_mat[1, 4]))  # PRO has NA
})

test_that("adjust_protein handles glycopeptides not present in protein matrix", {
  # Create test data where some glycopeptides don't have corresponding proteins
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:4),
    group = c("A", "A", "B", "B")
  )
  
  var_info <- tibble::tibble(
    variable = paste0("GP", 1:5),
    protein = c("PRO1", "PRO2", "PRO3", "PRO4", "PRO5"),  # 5 proteins
    protein_site = c(1L, 2L, 3L, 4L, 5L),
    peptide = paste0("PEP", 1:5),
    glycan_composition = paste0("H", 3:7, "N2")
  )
  
  expr_mat <- matrix(1:20, nrow = 5, ncol = 4)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )
  
  # Create protein expression matrix with only 3 out of 5 proteins
  pro_expr_mat <- matrix(1:12, nrow = 3, ncol = 4)
  colnames(pro_expr_mat) <- sample_info$sample
  rownames(pro_expr_mat) <- c("PRO1", "PRO3", "PRO5")  # Missing PRO2 and PRO4
  
  # Test that the function provides informative message and filters correctly
  expect_message(
    result <- adjust_protein(exp, pro_expr_mat, method = "ratio"),
    "Dropped 2 glycopeptides"
  )
  
  # Should only keep glycopeptides with proteins present in pro_expr_mat
  expect_equal(nrow(result$expr_mat), 3)  # GP1, GP3, GP5
  expect_equal(nrow(result$var_info), 3)
  expect_equal(result$var_info$variable, c("GP1", "GP3", "GP5"))
  expect_equal(result$var_info$protein, c("PRO1", "PRO3", "PRO5"))
  
  # Check that sample info is preserved
  expect_equal(result$sample_info, exp$sample_info)
  
  # Test the same with regression method
  expect_message(
    result_reg <- adjust_protein(exp, pro_expr_mat, method = "reg"),
    "Dropped 2 glycopeptides"
  )
  
  expect_equal(nrow(result_reg$expr_mat), 3)
  expect_equal(result_reg$var_info$variable, c("GP1", "GP3", "GP5"))
})
