test_that("infer_protein works with parsimony method", {
  # Create test data with shared proteins
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:3),
    group = c("A", "B", "A")
  )
  
  var_info <- tibble::tribble(
    ~variable, ~peptide, ~proteins, ~genes, ~protein_sites, ~glycan_composition,
    "GP1", "AAANAAK", "PRO1", "GENE1", "24", "H5N2",
    "GP2", "BBBNAAB", "PRO2", "GENE2", "30", "H3N2", 
    "GP3", "CCCNAAC", "PRO1;PRO3", "GENE1;GENE3", "40;45", "H4N2",
    "GP4", "DDDNAAD", "PRO2;PRO3", "GENE2;GENE3", "50;55", "H5N3",
    "GP5", "EEENAAE", "PRO3", "GENE3", "60", "H3N2"
  )
  
  expr_mat <- matrix(
    c(1, 2, 3, 4, 5,
      6, 7, 8, 9, 10,
      11, 12, 13, 14, 15),
    nrow = 5, ncol = 3
  )
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Test parsimony method
  result <- suppressMessages(infer_protein(exp, method = "parsimony"))
  
  # Check that the result is still an experiment
  expect_s3_class(result, "glyexp_experiment")
  
  # Check that the original columns are removed and new ones are added
  expect_false("proteins" %in% colnames(result$var_info))
  expect_false("genes" %in% colnames(result$var_info))
  expect_false("protein_sites" %in% colnames(result$var_info))
  
  expect_true("protein" %in% colnames(result$var_info))
  expect_true("gene" %in% colnames(result$var_info))
  expect_true("protein_site" %in% colnames(result$var_info))
  
  # Check that protein_site is integer
  expect_type(result$var_info$protein_site, "integer")
  
  # Check that each glycopeptide is assigned to exactly one protein
  expect_equal(length(result$var_info$protein), 5)
  expect_true(all(!is.na(result$var_info$protein)))
  expect_true(all(result$var_info$protein != ""))
  
  # Check that the assignment makes sense for parsimony
  # All three proteins PRO1, PRO2, PRO3 should be selected
  proteins_used <- unique(result$var_info$protein)
  expect_true(all(proteins_used %in% c("PRO1", "PRO2", "PRO3")))
  
  # Check specific assignments based on coverage count
  # PRO3 covers 3 glycopeptides (GP3, GP4, GP5), so GP3 and GP4 should be assigned to PRO3
  expect_equal(result$var_info$protein[result$var_info$variable == "GP1"], "PRO1")  # Only PRO1 available
  expect_equal(result$var_info$protein[result$var_info$variable == "GP2"], "PRO2")  # Only PRO2 available
  expect_equal(result$var_info$protein[result$var_info$variable == "GP3"], "PRO3")  # PRO3 has higher coverage than PRO1
  expect_equal(result$var_info$protein[result$var_info$variable == "GP4"], "PRO3")  # PRO3 has higher coverage than PRO2
  expect_equal(result$var_info$protein[result$var_info$variable == "GP5"], "PRO3")  # Only PRO3 available
  
  # Expression matrix should remain unchanged
  expect_equal(result$expr_mat, exp$expr_mat)
  expect_equal(result$sample_info, exp$sample_info)
})

test_that("infer_protein works with unique method", {
  # Create test data
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:3),
    group = c("A", "B", "A")
  )
  
  var_info <- tibble::tribble(
    ~variable, ~peptide, ~proteins, ~genes, ~protein_sites, ~glycan_composition,
    "GP1", "AAANAAK", "PRO1", "GENE1", "24", "H5N2",
    "GP2", "BBBNAAB", "PRO2", "GENE2", "30", "H3N2", 
    "GP3", "CCCNAAC", "PRO1;PRO3", "GENE1;GENE3", "40;45", "H4N2",
    "GP4", "DDDNAAD", "PRO2;PRO3", "GENE2;GENE3", "50;55", "H5N3",
    "GP5", "EEENAAE", "PRO3", "GENE3", "60", "H3N2"
  )
  
  expr_mat <- matrix(
    c(1, 2, 3, 4, 5,
      6, 7, 8, 9, 10,
      11, 12, 13, 14, 15),
    nrow = 5, ncol = 3
  )
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Test unique method
  result <- suppressMessages(infer_protein(exp, method = "unique"))
  
  # Should only keep glycopeptides that don't contain ";"
  expect_equal(nrow(result$var_info), 3)  # GP1, GP2, GP5
  expect_equal(result$var_info$variable, c("GP1", "GP2", "GP5"))
  
  # Check column names
  expect_true("protein" %in% colnames(result$var_info))
  expect_true("gene" %in% colnames(result$var_info))
  expect_true("protein_site" %in% colnames(result$var_info))
  
  expect_false("proteins" %in% colnames(result$var_info))
  expect_false("genes" %in% colnames(result$var_info))
  expect_false("protein_sites" %in% colnames(result$var_info))
})

test_that("infer_protein handles tie-breaking correctly", {
  # Create test data where proteins have equal coverage
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:3)
  )
  
  var_info <- tibble::tribble(
    ~variable, ~peptide, ~proteins, ~genes, ~protein_sites, ~glycan_composition,
    "GP1", "AAANAAK", "PRO1;PRO2", "GENE1;GENE2", "24;30", "H5N2",
    "GP2", "BBBNAAB", "PRO1", "GENE1", "40", "H3N2", 
    "GP3", "CCCNAAC", "PRO2", "GENE2", "50", "H4N2"
  )
  
  expr_mat <- matrix(1:9, nrow = 3, ncol = 3)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Both PRO1 and PRO2 cover 2 glycopeptides each: PRO1(GP1,GP2), PRO2(GP1,GP3)
  # For GP1, both are available with equal coverage, should choose first one (PRO1)
  result <- suppressMessages(infer_protein(exp, method = "parsimony"))
  
  expect_equal(result$var_info$protein[result$var_info$variable == "GP1"], "PRO1")  # First in tie
  expect_equal(result$var_info$protein[result$var_info$variable == "GP2"], "PRO1")  # Only PRO1 available
  expect_equal(result$var_info$protein[result$var_info$variable == "GP3"], "PRO2")  # Only PRO2 available
})

test_that("infer_protein handles edge cases", {
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:2)
  )
  
  var_info <- tibble::tribble(
    ~variable, ~peptide, ~proteins, ~genes, ~protein_sites, ~glycan_composition,
    "GP1", "AAANAAK", "PRO1", "GENE1", "24", "H5N2"
  )
  
  expr_mat <- matrix(c(1, 2), nrow = 1, ncol = 2)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Test with single protein (no sharing)
  result <- suppressMessages(infer_protein(exp, method = "parsimony"))
  
  expect_equal(result$var_info$protein, "PRO1")
  expect_equal(result$var_info$gene, "GENE1")
  expect_equal(result$var_info$protein_site, 24L)
})

test_that("infer_protein works with share method", {
  # Create test data with shared proteins
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:3),
    group = c("A", "B", "A")
  )
  
  var_info <- tibble::tribble(
    ~variable, ~peptide, ~proteins, ~genes, ~protein_sites, ~glycan_composition,
    "GP1", "AAANAAK", "PRO1", "GENE1", "24", "H5N2",
    "GP2", "BBBNAAB", "PRO2", "GENE2", "30", "H3N2", 
    "GP3", "CCCNAAC", "PRO1;PRO3", "GENE1;GENE3", "40;45", "H4N2",
    "GP4", "DDDNAAD", "PRO2;PRO3", "GENE2;GENE3", "50;55", "H5N3"
  )
  
  expr_mat <- matrix(
    c(10, 20, 30, 40,   # Sample S1
      100, 200, 300, 400, # Sample S2  
      1000, 2000, 3000, 4000), # Sample S3
    nrow = 4, ncol = 3
  )
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  # Test share method
  result <- suppressMessages(infer_protein(exp, method = "share"))
  
  # Check that the result is still an experiment
  expect_s3_class(result, "glyexp_experiment")
  
  # Check that we have more rows now (shared glycopeptides are split)
  expect_gt(nrow(result$var_info), nrow(var_info))
  
  # Should have 6 rows: GP1(1) + GP2(1) + GP3(2) + GP4(2) = 6
  expect_equal(nrow(result$var_info), 6)
  expect_equal(nrow(result$expr_mat), 6)
  
  # Check column names
  expect_true("protein" %in% colnames(result$var_info))
  expect_true("gene" %in% colnames(result$var_info))
  expect_true("protein_site" %in% colnames(result$var_info))
  
  expect_false("proteins" %in% colnames(result$var_info))
  expect_false("genes" %in% colnames(result$var_info))
  expect_false("protein_sites" %in% colnames(result$var_info))
  
  # Check that protein_site is integer
  expect_type(result$var_info$protein_site, "integer")
  
  # Check variable names for shared glycopeptides
  expected_variables <- c("GP1_PRO1", "GP2_PRO2", "GP3_PRO1", "GP3_PRO3", "GP4_PRO2", "GP4_PRO3")
  expect_setequal(result$var_info$variable, expected_variables)
  
  # Check expression values are shared correctly
  # GP3 original: [30, 300, 3000], should be split into [15, 150, 1500] for each protein
  gp3_pro1_idx <- which(result$var_info$variable == "GP3_PRO1")
  gp3_pro3_idx <- which(result$var_info$variable == "GP3_PRO3")
  
  expect_equal(as.numeric(result$expr_mat[gp3_pro1_idx, ]), c(15, 150, 1500))
  expect_equal(as.numeric(result$expr_mat[gp3_pro3_idx, ]), c(15, 150, 1500))
  
  # Check that the sum of shared expressions equals original
  expect_equal(as.numeric(result$expr_mat[gp3_pro1_idx, ] + result$expr_mat[gp3_pro3_idx, ]), c(30, 300, 3000))
  
  # Sample info should remain unchanged
  expect_equal(result$sample_info, exp$sample_info)
})

test_that("infer_protein validates input", {
  # Test with wrong experiment type
  sample_info <- tibble::tibble(sample = "S1")
  var_info <- tibble::tibble(variable = "V1")
  expr_mat <- matrix(1, nrow = 1, ncol = 1)
  colnames(expr_mat) <- "S1"
  rownames(expr_mat) <- "V1"
  
  exp <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycomics",  # Wrong type
    glycan_type = "N"
  )
  
  expect_error(infer_protein(exp, method = "parsimony"), "Only glycoproteomics data is supported")
  
  # Test with missing proteins column
  exp2 <- glyexp::experiment(
    expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  
  expect_error(infer_protein(exp2, method = "parsimony"), "The.*proteins.*column does not exist")
}) 