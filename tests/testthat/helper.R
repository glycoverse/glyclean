simple_exp <- function(n_var, n_samp) {
  sample_info <- tibble::tibble(sample = paste0("S", 1:n_samp))
  var_info <- tibble::tibble(variable = paste0("V", 1:n_var))
  expr_mat <- matrix(1:(n_var * n_samp), nrow = n_var)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  glyexp::experiment("test_exp", expr_mat, sample_info, var_info)
}


complex_exp <- function() {
  sample_info <- tibble::tibble(
    sample = paste0("S", 1:6),
    group = c("A", "A", "A", "B", "B", "B"),
  )
  var_info <- tibble::tribble(
    ~peptide, ~proteins, ~genes, ~glycan_composition, ~glycan_structure, ~peptide_site, ~protein_sites, ~charge, ~modifications,
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "",
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "",  # same as row 1
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H)(H(H(H))))))", 4, 24, 2, "",  # different structure
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "6,Carbamidomethyl[C]",  # different modifications
    "AAANAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 3, "",  # different charge
    "AAANAAKAAK", "PRO1", "GENE1", "H5N2", "(N(N(H(H(H))(H(H)))))", 4, 24, 2, "",  # different peptide
    "AAANAAK", "PRO1", "GENE1", "H3N2", "(N(N(H(H(H)))))", 4, 24, 2, "",  # different glycan (both composition and structure)
  )
  var_info <- dplyr::mutate(
    var_info,
    variable = paste0("PSM", 1:nrow(var_info)),
    .before = 1
  )
  nrow <- nrow(var_info)
  ncol <- nrow(sample_info)
  exp_mat <- matrix(1.0: (nrow * ncol), nrow = nrow, ncol = ncol)
  colnames(exp_mat) <- sample_info$sample
  rownames(exp_mat) <- var_info$variable
  glyexp::experiment("test_exp", exp_mat, sample_info, var_info)
}
