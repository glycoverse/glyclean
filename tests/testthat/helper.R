simple_exp <- function(n_var, n_samp) {
  sample_info <- tibble::tibble(sample = paste0("S", 1:n_samp))
  var_info <- tibble::tibble(variable = paste0("V", 1:n_var))
  expr_mat <- matrix(1:(n_var * n_samp), nrow = n_var)
  colnames(expr_mat) <- sample_info$sample
  rownames(expr_mat) <- var_info$variable
  glyexp::experiment("test_exp", expr_mat, sample_info, var_info)
}
