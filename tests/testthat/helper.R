test_glycomic_se <- function(
  expr_mat,
  sample_info = NULL,
  var_info = NULL,
  glycan_type = "N",
  ...
) {
  if (is.null(colnames(expr_mat))) {
    colnames(expr_mat) <- paste0("S", seq_len(ncol(expr_mat)))
  }
  if (is.null(rownames(expr_mat))) {
    rownames(expr_mat) <- paste0("V", seq_len(nrow(expr_mat)))
  }
  if (is.null(sample_info)) {
    sample_info <- tibble::tibble(sample = colnames(expr_mat))
  }
  if (is.null(var_info)) {
    var_info <- tibble::tibble(variable = rownames(expr_mat))
  }
  if (
    !"glycan_composition" %in% colnames(var_info) &&
      !"glycan_structure" %in% colnames(var_info)
  ) {
    var_info$glycan_composition <- rep("H5N2", nrow(var_info))
  }
  if (
    "glycan_composition" %in%
      colnames(var_info) &&
      !glyrepr::is_glycan_composition(var_info$glycan_composition)
  ) {
    var_info$glycan_composition <- glyrepr::as_glycan_composition(
      var_info$glycan_composition
    )
  }
  if (
    "glycan_structure" %in%
      colnames(var_info) &&
      !glyrepr::is_glycan_structure(var_info$glycan_structure)
  ) {
    var_info$glycan_structure <- glyrepr::as_glycan_structure(
      var_info$glycan_structure
    )
  }

  sample_names <- sample_info$sample
  variable_names <- var_info$variable
  sample_info$sample <- NULL
  var_info$variable <- NULL
  glyexp::GlycomicSE(
    expr_mat,
    colData = S4Vectors::DataFrame(sample_info, row.names = sample_names),
    rowData = S4Vectors::DataFrame(var_info, row.names = variable_names),
    metadata = list(glycan_type = glycan_type)
  )
}

test_glycoproteomic_se <- function(
  expr_mat,
  sample_info = NULL,
  var_info = NULL,
  glycan_type = "N",
  ...
) {
  if (is.null(var_info)) {
    var_info <- tibble::tibble(variable = rownames(expr_mat))
  }
  if (!"protein" %in% colnames(var_info)) {
    var_info$protein <- paste0("P", seq_len(nrow(var_info)))
  }
  if (!"protein_site" %in% colnames(var_info)) {
    var_info$protein_site <- as.integer(seq_len(nrow(var_info)))
  }
  se <- test_glycomic_se(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    glycan_type = glycan_type
  )
  glyexp::as_glycoproteomic_se(se)
}

simple_exp <- function(n_var, n_samp) {
  sample_info <- tibble::tibble(sample = paste0("S", seq_len(n_samp)))
  var_info <- tibble::tibble(variable = paste0("V", seq_len(n_var)))
  expr_mat <- matrix(seq_len(n_var * n_samp), nrow = n_var)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  test_glycomic_se(expr_mat, sample_info, var_info)
}

legacy_exp <- function(
  n_var,
  n_samp,
  exp_type = "others",
  glycan_type = NULL
) {
  sample_info <- tibble::tibble(sample = paste0("S", seq_len(n_samp)))
  var_info <- tibble::tibble(variable = paste0("V", seq_len(n_var)))
  if (exp_type == "traitproteomics") {
    var_info$protein <- paste0("P", seq_len(n_var))
    var_info$protein_site <- as.integer(seq_len(n_var))
  }
  expr_mat <- matrix(seq_len(n_var * n_samp), nrow = n_var)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  glyexp::experiment(
    expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = exp_type,
    glycan_type = glycan_type
  )
}

complex_exp <- function() {
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:6),
    group = factor(c("A", "A", "A", "B", "B", "B")),
  )
  var_info <- tibble::tribble(
    ~peptide     , ~protein , ~gene   , ~glycan_composition , ~glycan_structure       , ~peptide_site , ~protein_site , ~charge , ~modifications         ,
    "AAANAAK"    , "PRO1"   , "GENE1" , "H5N2"              , "(N(N(H(H(H))(H(H)))))" , 4L            , 24L           , 2L      , ""                     ,
    "AAANAAK"    , "PRO1"   , "GENE1" , "H5N2"              , "(N(N(H(H(H))(H(H)))))" , 4L            , 24L           , 2L      , ""                     , # same as row 1
    "AAANAAK"    , "PRO1"   , "GENE1" , "H5N2"              , "(N(N(H(H(H))(H(H)))))" , 5L            , 25L           , 2L      , ""                     , # different site
    "AAANAAK"    , "PRO1"   , "GENE1" , "H5N2"              , "(N(N(H(H)(H(H(H))))))" , 4L            , 24L           , 2L      , ""                     , # different structure
    "AAANAAK"    , "PRO1"   , "GENE1" , "H5N2"              , "(N(N(H(H(H))(H(H)))))" , 4L            , 24L           , 2L      , "6,Carbamidomethyl[C]" , # different modifications
    "AAANAAK"    , "PRO1"   , "GENE1" , "H5N2"              , "(N(N(H(H(H))(H(H)))))" , 4L            , 24L           , 3L      , ""                     , # different charge
    "AAANAAKAAK" , "PRO1"   , "GENE1" , "H5N2"              , "(N(N(H(H(H))(H(H)))))" , 4L            , 24L           , 2L      , ""                     , # different peptide
    "AAANAAK"    , "PRO1"   , "GENE1" , "H3N2"              , "(N(N(H(H(H)))))"       , 4L            , 24L           , 2L      , ""                     , # different glycan (both composition and structure)
  )
  var_info <- dplyr::mutate(
    var_info,
    variable = paste0("PSM", 1:nrow(var_info)),
    .before = 1
  )
  var_info$glycan_composition <- glyrepr::as_glycan_composition(
    var_info$glycan_composition
  )
  real_structures <- SummarizedExperiment::rowData(
    glyexp::as_glycoproteomic_se(glyexp::real_experiment)
  )$glycan_structure
  var_info$glycan_structure <- real_structures[c(1, 1, 1, 2, 1, 1, 1, 3)]
  nrow <- nrow(var_info)
  ncol <- nrow(sample_info)
  exp_mat <- matrix(1.0:(nrow * ncol), nrow = nrow, ncol = ncol)
  colnames(exp_mat) <- sample_info$sample
  rownames(exp_mat) <- var_info$variable
  test_glycoproteomic_se(
    exp_mat,
    sample_info = sample_info,
    var_info = var_info,
    glycan_type = "N",
    coerce_col_types = FALSE,
    check_col_types = FALSE
  )
}

real_exp <- function() {
  glyexp::real_experiment |>
    glyexp::slice_head_var(n = 100) |>
    glyexp::as_glycoproteomic_se()
}

real_glycomic_exp <- function() {
  glyexp::as_glycomic_se(glyexp::real_experiment2)
}

expect_glyco_se <- function(x) {
  testthat::expect_true(
    glyexp::is_glycomic_se(x) || glyexp::is_glycoproteomic_se(x)
  )
}

sanitize_cv_snapshot <- function(x) {
  x |>
    stringr::str_replace_all(
      "CV = [-+]?\\d*\\.?\\d+(?:e[-+]?\\d+)?",
      "CV = CV_VALUE"
    )
}

simple_glycomic_se <- function(n_var = 4, n_samp = 6) {
  abundance <- matrix(
    seq_len(n_var * n_samp),
    nrow = n_var,
    dimnames = list(
      paste0("V", seq_len(n_var)),
      paste0("S", seq_len(n_samp))
    )
  )
  row_data <- S4Vectors::DataFrame(
    glycan_composition = glyrepr::as_glycan_composition(
      rep("H5N2", n_var)
    )
  )
  col_data <- S4Vectors::DataFrame(
    group = factor(rep(c("A", "B"), length.out = n_samp)),
    row.names = colnames(abundance)
  )

  glyexp::GlycomicSE(
    abundance,
    rowData = row_data,
    colData = col_data,
    metadata = list(glycan_type = "N")
  )
}

simple_glycoproteomic_se <- function(n_var = 20) {
  glyexp::real_experiment |>
    glyexp::slice_head_var(n = n_var) |>
    glyexp::as_glycoproteomic_se()
}

plain_se <- function(x) {
  SummarizedExperiment::SummarizedExperiment(
    assays = SummarizedExperiment::assays(x),
    rowData = SummarizedExperiment::rowData(x),
    colData = SummarizedExperiment::colData(x),
    metadata = S4Vectors::metadata(x)
  )
}

simple_glycoproteomic_exp <- function(n_var, n_samp) {
  sample_info <- tibble::tibble(sample = paste0("S", seq_len(n_samp)))
  var_info <- tibble::tibble(variable = paste0("V", seq_len(n_var)))
  expr_mat <- matrix(seq_len(n_var * n_samp), nrow = n_var)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  test_glycoproteomic_se(expr_mat, sample_info, var_info)
}
