#' Protein Inference for Shared Glycopeptides
#'
#' @description
#' Resolves protein assignment ambiguity when glycopeptides are shared across 
#' multiple proteins. Provides three strategies: retain only unique assignments, 
#' apply parsimony principle, or split shared peptides among all candidate proteins.
#'
#' The function processes semicolon-delimited protein annotations in the `proteins`, 
#' `genes`, and `protein_sites` columns (e.g., "PRO1;PRO2" for shared peptides) and 
#' converts them to singular assignments with corresponding `protein`, `gene`, and 
#' `protein_site` columns.
#' 
#' @details
#' This function provides three different strategies for handling shared glycopeptides:
#' 
#' ## Unique Method
#' 
#' The **unique** method is the most conservative approach. It only retains glycopeptides 
#' that are uniquely assigned to a single protein (i.e., those without ";" in the proteins column).
#' 
#' **Algorithm**: Simple filtering based on protein column content.
#' 
#' **Use cases**: 
#' - When you need high confidence in protein assignments
#' - For exploratory analysis where shared peptides might introduce noise
#' - When computational simplicity is preferred
#' 
#' **Trade-offs**: 
#' - Pros: No ambiguity, clean results
#' - Cons: Loss of data (shared glycopeptides are discarded)
#' 
#' ## Parsimony Method  
#' 
#' The **parsimony** method uses a greedy set cover algorithm to find the minimal set of 
#' proteins that can explain all observed glycopeptides. It then assigns each shared 
#' glycopeptide to the protein with the highest overall coverage.
#' 
#' **Algorithm**:
#' 1. Greedy selection: Iteratively choose the protein that covers the most uncovered glycopeptides
#' 2. Coverage-based assignment: Assign each glycopeptide to the selected protein with highest total coverage
#' 3. Tie-breaking: If coverage is equal, select the first protein in the original order
#' 
#' **Use cases**:
#' - Standard protein inference in glycoproteomics
#' - When you want to retain all data while minimizing protein redundancy
#' - For quantitative analysis where protein count matters
#' 
#' **Trade-offs**:
#' - Pros: Retains all glycopeptides, uses minimal protein set, biologically motivated
#' - Cons: Some assignments may be arbitrary for highly shared peptides
#' 
#' ## Share Method
#' 
#' The **share** method assumes that shared glycopeptides are equally contributed by all 
#' associated proteins. Each shared glycopeptide is split into multiple entries, with 
#' expression values divided equally among the proteins.
#' 
#' **Algorithm**:
#' 1. Split each glycopeptide into separate entries for each associated protein
#' 2. Divide expression values by the number of associated proteins
#' 3. Create unique variable names using the format "original_variable_protein"
#' 
#' **Use cases**:
#' - When you need to preserve complete protein-glycopeptide relationships
#' - For protein-level quantification where each protein's contribution matters
#' - When studying protein isoforms or splice variants
#' 
#' **Trade-offs**:
#' - Pros: Complete information preservation, expression conservation, explicit modeling of sharing
#' - Cons: Increased data size, assumption of equal contribution may not reflect biology
#' 
#' ## Output Format
#' 
#' All methods convert the plural columns (`proteins`, `genes`, `protein_sites`) to 
#' singular equivalents (`protein`, `gene`, `protein_site`), where `protein_site` 
#' is converted to integer type. The choice of method depends on your specific 
#' analysis goals and tolerance for data loss versus ambiguity.
#' 
#' @param exp A `glyexp::experiment()` containing glycoproteomics data.
#' @param method The method to use for protein inference.
#'  Either `unique`, `parsimony`, or `share`.
#'  Default is `parsimony`.
#'
#' @return A `glyexp::experiment()` containing with protein infered.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyselect all_of
infer_protein <- function(exp, method = c("parsimony", "share", "unique")) {
  checkmate::assert_class(exp, "glyexp_experiment")
  method <- rlang::arg_match(method)

  if (glyexp::get_exp_type(exp) != "glycoproteomics") {
    cli::cli_abort("Only glycoproteomics data is supported.")
  }
  if (!"proteins" %in% colnames(exp$var_info)) {
    if ("protein" %in% colnames(exp$var_info)) {
      cli::cli_abort(c(
        "The {.var proteins} column does not exist.",
        "i" = "A {.var protein} column exists instead. Maybe protein inference has been performed."
      ))
    }
    cli::cli_abort("The {.var proteins} column does not exist.")
  }

  switch(method,
    unique = .infer_pro_unique(exp),
    parsimony = .infer_pro_parsimony(exp),
    share = .infer_pro_share(exp)
  )
}

.infer_pro_unique <- function(exp) {
  exp %>%
    glyexp::filter_var(!stringr::str_detect(.data$proteins, ";")) %>%
    glyexp::rename_var(all_of(c("protein" = "proteins", "gene" = "genes", "protein_site" = "protein_sites")))
}

.infer_pro_parsimony <- function(exp) {
  var_info <- exp$var_info
  
  # Parse the proteins, genes, and protein_sites columns
  proteins_list <- stringr::str_split(var_info$proteins, ";")
  genes_list <- stringr::str_split(var_info$genes, ";")
  protein_sites_list <- stringr::str_split(var_info$protein_sites, ";")
  
  # Create a mapping from protein to glycopeptide indices
  protein_to_gp <- proteins_list %>%
    purrr::imap(~ tibble::tibble(protein = .x, gp_idx = .y)) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.data$protein) %>%
    dplyr::summarise(gp_indices = list(.data$gp_idx), .groups = "drop") %>%
    (function(x) stats::setNames(x$gp_indices, x$protein))
  
  # Greedy set cover algorithm
  uncovered_gps <- seq_len(nrow(var_info))
  selected_proteins <- character(0)
  
  while (length(uncovered_gps) > 0) {
    # Find the protein that covers the most uncovered glycopeptides
    available_proteins <- setdiff(names(protein_to_gp), selected_proteins)
    
    if (length(available_proteins) == 0) break
    
    # Calculate coverage for all available proteins
    coverages <- available_proteins %>%
      purrr::map_int(~ length(intersect(protein_to_gp[[.x]], uncovered_gps))) %>%
      purrr::set_names(available_proteins)
    
    # Find the protein with maximum coverage
    max_coverage <- max(coverages)
    if (max_coverage == 0) break
    
    best_protein <- names(coverages)[which.max(coverages)]
    
    # Add the best protein to the selected set
    selected_proteins <- c(selected_proteins, best_protein)
    
    # Remove covered glycopeptides from uncovered set
    uncovered_gps <- setdiff(uncovered_gps, protein_to_gp[[best_protein]])
  }
  
  # Calculate coverage count for each selected protein
  protein_coverage_count <- selected_proteins %>%
    purrr::map_int(~ length(protein_to_gp[[.x]])) %>%
    purrr::set_names(selected_proteins)
  
  # Assign each glycopeptide to the selected protein with most coverage
  assignments <- purrr::pmap(
    list(proteins_list, genes_list, protein_sites_list),
    function(prots, genes, sites) {
      # Find available selected proteins for this glycopeptide
      available_selected <- intersect(prots, selected_proteins)
      
      if (length(available_selected) > 0) {
        # Choose the protein with highest coverage count
        # If tie, choose the first one
        best_protein <- available_selected[which.max(protein_coverage_count[available_selected])]
        
        # Find the corresponding gene and site
        best_idx <- which(prots == best_protein)[1]
        
        list(
          protein = best_protein,
          gene = genes[best_idx],
          site = sites[best_idx]
        )
      } else {
        list(protein = NA_character_, gene = NA_character_, site = NA_character_)
      }
    }
  )
  
  assigned_protein <- purrr::map_chr(assignments, ~ .x$protein)
  assigned_gene <- purrr::map_chr(assignments, ~ .x$gene) 
  assigned_site <- purrr::map_chr(assignments, ~ .x$site)
  
  # Update the experiment
  new_var_info <- var_info %>%
    dplyr::select(-all_of(c("proteins", "genes", "protein_sites"))) %>%
    dplyr::mutate(
      protein = assigned_protein,
      gene = assigned_gene,
      protein_site = as.integer(assigned_site)
    )
  
  new_exp <- exp
  new_exp$var_info <- new_var_info
  new_exp
}

.infer_pro_share <- function(exp) {
  var_info <- exp$var_info
  expr_mat <- exp$expr_mat
  
  # Parse the proteins, genes, and protein_sites columns
  proteins_list <- stringr::str_split(var_info$proteins, ";")
  genes_list <- stringr::str_split(var_info$genes, ";")
  protein_sites_list <- stringr::str_split(var_info$protein_sites, ";")
  
  # Helper function to create a single protein entry from shared glycopeptide
  create_protein_entry <- function(protein, gene, site, idx, shared_expr) {
    new_variable <- paste0(var_info$variable[idx], "_", protein)
    
    # Create new var_info row
    new_var_row <- var_info[idx, , drop = FALSE]
    new_var_row$variable <- new_variable
    new_var_row <- new_var_row %>%
      dplyr::select(-all_of(c("proteins", "genes", "protein_sites"))) %>%
      dplyr::mutate(
        protein = protein,
        gene = gene,
        protein_site = as.integer(site)
      )
    
    # Create new expression row
    new_expr_row <- shared_expr
    rownames(new_expr_row) <- new_variable
    
    list(var_info = new_var_row, expr_row = new_expr_row)
  }
  
  # Helper function to process a single glycopeptide and split by proteins
  process_glycopeptide <- function(proteins, genes, sites, idx) {
    n_proteins <- length(proteins)
    original_expr <- expr_mat[idx, , drop = FALSE]
    shared_expr <- original_expr / n_proteins
    
    # Create rows for each protein
    purrr::pmap(
      list(protein = proteins, gene = genes, site = sites),
      ~ create_protein_entry(..1, ..2, ..3, idx, shared_expr)
    )
  }
  
  # Process each glycopeptide and split by proteins
  share_results <- purrr::pmap(
    list(
      proteins = proteins_list,
      genes = genes_list, 
      sites = protein_sites_list,
      idx = seq_len(nrow(var_info))
    ),
    process_glycopeptide
  ) %>%
    purrr::flatten()
  
  # Extract var_info and expression data
  new_var_info_list <- purrr::map(share_results, ~ .x$var_info)
  new_expr_rows <- purrr::map(share_results, ~ .x$expr_row)
  
  # Combine all new data
  new_var_info <- dplyr::bind_rows(new_var_info_list)
  new_expr_mat <- do.call(rbind, new_expr_rows)
  
  # Create new experiment
  new_exp <- exp
  new_exp$var_info <- new_var_info
  new_exp$expr_mat <- new_expr_mat
  new_exp
}
