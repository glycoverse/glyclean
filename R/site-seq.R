#' Add site-specific sequence information.
#'
#' @description
#' This function adds a new "site_sequence" column to the variable information table.
#' It is the protein sequence around the glycosylation site.
#' If the head and tail amino acids of the peptide sequence are insufficient, fill with "X".
#'
#' This function requires the following columns in the variable information tibble:
#' - "protein": The protein uniprot accession. If "proteins" is used,
#'   please use `infer_protein()` first.
#' - "protein_site": The site on the protein sequence. If "protein_sites" is used,
#'   please use `infer_protein()` first.
#'
#' @param exp A `glyexp::experiment()` object.
#' @param fasta A character string specifying the path to the FASTA file containing protein sequences.
#' @param n_aa The number of amino acids to the left and right of the glycosylation site.
#' For example, if `n_aa = 5`, the resulting sequence will contain 11 amino acids.
#'
#' @returns A `glyexp::experiment()` object with the new "site_sequence" column.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
add_site_seq <- function(exp, fasta, n_aa = 7) {
  .dispatch_on_input(
    exp,
    fun_exp = .add_site_seq_experiment,
    fun_mat = .add_site_seq_matrix,
    fasta = fasta,
    n_aa = n_aa
  )
}

.add_site_seq_experiment <- function(exp, fasta, n_aa = 7) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  checkmate::assert_string(fasta)
  checkmate::assert_file_exists(fasta)
  checkmate::assert_int(n_aa, lower = 1)
  
  # Check if required columns exist
  if (!"protein" %in% colnames(exp$var_info)) {
    if ("proteins" %in% colnames(exp$var_info)) {
      cli::cli_abort(c(
        "The {.field protein} column does not exist.",
        "i" = "A {.field proteins} column exists instead. Please call {.fn infer_protein} first."
      ))
    } else {
      cli::cli_abort("The {.field protein} column does not exist.")
    }
  }
  
  if (!"protein_site" %in% colnames(exp$var_info)) {
    if ("protein_sites" %in% colnames(exp$var_info)) {
      cli::cli_abort(c(
        "The {.field protein_site} column does not exist.",
        "i" = "A {.field protein_sites} column exists instead. Please call {.fn infer_protein} first."
      ))
    } else {
      cli::cli_abort("The {.field protein_site} column does not exist.")
    }
  }
  
  # Read FASTA file
  protein_seqs <- .read_fasta_file(fasta)
  
  # Extract site sequences
  var_info <- exp$var_info %>%
    dplyr::mutate(
      site_sequence = purrr::map2_chr(
        .data$protein, .data$protein_site,
        ~ .extract_site_sequence(.x, .y, protein_seqs, n_aa)
      )
    )
  
  # Update experiment
  new_exp <- exp
  new_exp$var_info <- var_info
  new_exp
}

.add_site_seq_matrix <- function(exp, fasta, n_aa = 7) {
  cli::cli_abort("The {.fn add_site_seq} function does not support matrix input.")
}

# Helper function to read FASTA file using seqinr
.read_fasta_file <- function(fasta_path) {
  # Use seqinr to read FASTA file
  fasta_seqs <- seqinr::read.fasta(fasta_path, seqtype = "AA", as.string = TRUE)
  
  if (length(fasta_seqs) == 0) {
    cli::cli_abort("No FASTA sequences found in file {.file {fasta_path}}")
  }
  
  # Extract sequences and convert to character strings
  protein_seqs <- purrr::map_chr(fasta_seqs, function(seq) {
    # seqinr::read.fasta with as.string = TRUE returns character vectors
    # Convert to uppercase and remove any spaces
    toupper(stringr::str_remove_all(seq, "\\s"))
  })
  
  # Extract protein IDs from names
  fasta_names <- names(fasta_seqs)
  
  # Try to extract UniProt accession from header
  # Handle different formats: >P12345, >sp|P12345|NAME, etc.
  protein_ids <- stringr::str_extract(fasta_names, "[A-Z0-9]{6,10}")
  
  # If no UniProt-style IDs found, use the full name
  if (all(is.na(protein_ids))) {
    protein_ids <- fasta_names
  }
  
  # Create named vector
  names(protein_seqs) <- protein_ids
  
  # Remove any sequences with missing IDs
  protein_seqs <- protein_seqs[!is.na(names(protein_seqs))]
  
  if (length(protein_seqs) == 0) {
    cli::cli_abort("No valid protein sequences found in FASTA file {.file {fasta_path}}")
  }
  
  protein_seqs
}

# Helper function to extract site sequence
.extract_site_sequence <- function(protein_id, site, protein_seqs, n_aa) {
  # Handle missing values
  if (is.na(protein_id) || is.na(site)) {
    return(paste0(rep("X", 2 * n_aa + 1), collapse = ""))
  }
  
  # Get protein sequence
  if (!protein_id %in% names(protein_seqs)) {
    return(paste0(rep("X", 2 * n_aa + 1), collapse = ""))
  }
  
  protein_seq <- protein_seqs[[protein_id]]
  seq_length <- nchar(protein_seq)
  
  # Check if site is within sequence
  if (site < 1 || site > seq_length) {
    return(paste0(rep("X", 2 * n_aa + 1), collapse = ""))
  }
  
  # Calculate start and end positions
  start_pos <- site - n_aa
  end_pos <- site + n_aa
  
  # Extract sequence with padding if necessary
  site_seq <- character(2 * n_aa + 1)
  
  for (i in 1:(2 * n_aa + 1)) {
    pos <- start_pos + i - 1
    if (pos < 1 || pos > seq_length) {
      site_seq[i] <- "X"
    } else {
      site_seq[i] <- substr(protein_seq, pos, pos)
    }
  }
  
  paste(site_seq, collapse = "")
}
