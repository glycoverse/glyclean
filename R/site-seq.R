#' Add site-specific sequence information.
#'
#' @description
#' This function adds a new "site_sequence" column to the variable information table.
#' It is the protein sequence around the glycosylation site.
#' If the head and tail amino acids of the peptide sequence are insufficient, fill with "X".
#'
#' This function requires the following columns in the variable information tibble:
#' - "protein": The protein uniprot accession.
#' - "protein_site": The site on the protein sequence.
#'
#' @param exp A [glyexp::experiment()] object with "glycoproteomics" type.
#' @param fasta A character string specifying the path to the FASTA file containing protein sequences.
#' @param n_aa The number of amino acids to the left and right of the glycosylation site.
#' For example, if `n_aa = 5`, the resulting sequence will contain 11 amino acids.
#'
#' @returns A [glyexp::experiment()] object with the new "site_sequence" column.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
add_site_seq <- function(exp, fasta, n_aa = 7) {
  UseMethod("add_site_seq")
}

#' @rdname add_site_seq
#' @export
add_site_seq.glyexp_experiment <- function(exp, fasta, n_aa = 7) {
  .add_site_seq_experiment(exp, fasta = fasta, n_aa = n_aa)
}

#' @rdname add_site_seq
#' @export
add_site_seq.default <- function(exp, fasta, n_aa = 7) {
  cli::cli_abort(c(
    "{.arg exp} must be a {.cls glyexp_experiment} object.",
    "x" = "Got {.cls {class(exp)}}."
  ))
}

.add_site_seq_experiment <- function(exp, fasta, n_aa = 7) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  if (glyexp::get_exp_type(exp) != "glycoproteomics") {
    cli::cli_abort(c(
      "The experiment type must be {.val glycoproteomics}.",
      "x" = "Got {.val {glyexp::get_exp_type(exp)}}."
    ))
  }
  is_file <- checkmate::test_file_exists(fasta)
  is_named_char <- is.character(fasta) && !is.null(names(fasta))

  if (!is_file && !is_named_char) {
    cli::cli_abort(
      "fasta must be a file path or a named character vector."
    )
  }
  checkmate::assert_int(n_aa, lower = 1)

  # Check if required columns exist
  if (!"protein" %in% colnames(exp$var_info)) {
    cli::cli_abort("The {.field protein} column does not exist.")
  }

  if (!"protein_site" %in% colnames(exp$var_info)) {
    cli::cli_abort("The {.field protein_site} column does not exist.")
  }

  # Read FASTA file or use named character vector
  if (checkmate::test_file_exists(fasta)) {
    protein_seqs <- .read_fasta_file(fasta)
  } else {
    protein_seqs <- .validate_protein_seqs(fasta)
  }
  
  # Check protein matching and provide diagnostic information using purrr
  unique_proteins <- exp$var_info$protein %>%
    unique() %>%
    purrr::discard(is.na)
  
  # Split proteins into found and missing using purrr
  protein_status <- unique_proteins %>%
    purrr::set_names() %>%
    purrr::map_lgl(~ .x %in% names(protein_seqs))
  
  found_proteins <- protein_status %>%
    purrr::keep(identity) %>%
    names()
  
  missing_proteins <- protein_status %>%
    purrr::discard(identity) %>%
    names()
  
  cli::cli_inform(c(
    "i" = "FASTA file contains {length(protein_seqs)} protein sequences",
    "i" = "Found {length(found_proteins)} / {length(unique_proteins)} proteins from experiment in FASTA file"
  ))
  
  if (length(missing_proteins) > 0) {
    # Format missing proteins display
    missing_display <- if (length(missing_proteins) <= 5) {
      stringr::str_c(missing_proteins, collapse = ", ")
    } else {
      stringr::str_c(c(missing_proteins[1:5], "..."), collapse = ", ")
    }
    
    # Format message based on number of missing proteins
    message_text <- if (length(missing_proteins) <= 5) {
      "Missing proteins: {missing_display}"
    } else {
      "Missing {length(missing_proteins)} proteins (showing first 5): {missing_display}"
    }
    
    cli::cli_inform(c("!" = message_text))
  }
  
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

# Helper function to read FASTA file using seqinr
.read_fasta_file <- function(fasta_path) {
  # Use seqinr to read FASTA file
  fasta_seqs <- seqinr::read.fasta(fasta_path, seqtype = "AA", as.string = TRUE)
  
  if (length(fasta_seqs) == 0) {
    cli::cli_abort("No FASTA sequences found in file {.file {fasta_path}}")
  }
  
  # Extract sequences and convert to character strings using purrr
  protein_seqs <- purrr::map_chr(fasta_seqs, ~ {
    # seqinr::read.fasta with as.string = TRUE returns character vectors
    # Convert to uppercase and remove any spaces using stringr
    .x %>%
      stringr::str_to_upper() %>%
      stringr::str_remove_all("\\s")
  })
  
  # Extract protein IDs from names using stringr
  fasta_names <- names(fasta_seqs)
  
  # Try to extract UniProt accession from header using stringr
  # Handle different formats: >P12345, >sp|P12345|NAME, etc.
  extracted_ids <- fasta_names %>%
    stringr::str_extract("[A-Z0-9]{6,10}")
  
  # If no UniProt-style IDs found, use full name; otherwise use extracted IDs
  protein_ids <- if (all(is.na(extracted_ids))) {
    fasta_names
  } else {
    extracted_ids
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

#' Validate and normalize a named character vector of protein sequences
#' @keywords internal
.validate_protein_seqs <- function(seqs) {
  if (any(nchar(seqs) == 0)) {
    cli::cli_abort("All protein sequences must be non-empty strings.")
  }
  purrr::map_chr(seqs, ~ .x %>%
    stringr::str_to_upper() %>%
    stringr::str_remove_all("\\s"))
}

# Helper function to extract site sequence
.extract_site_sequence <- function(protein_id, site, protein_seqs, n_aa) {
  # Handle missing values
  if (is.na(protein_id) || is.na(site)) {
    return(NA_character_)
  }
  
  # Get protein sequence
  if (!protein_id %in% names(protein_seqs)) {
    return(NA_character_)
  }
  
  protein_seq <- protein_seqs[[protein_id]]
  seq_length <- nchar(protein_seq)
  
  # Check if site is within sequence
  if (site < 1 || site > seq_length) {
    return(NA_character_)
  }
  
  # Calculate start and end positions
  start_pos <- site - n_aa
  end_pos <- site + n_aa
  
  # Create position vector for the sequence window
  positions <- start_pos:end_pos
  
  # Extract amino acids using purrr::map_chr and stringr
  site_seq <- purrr::map_chr(positions, function(pos) {
    if (pos < 1 || pos > seq_length) {
      "X"
    } else {
      stringr::str_sub(protein_seq, pos, pos)
    }
  })
  
  # Collapse into single string
  stringr::str_c(site_seq, collapse = "")
}
