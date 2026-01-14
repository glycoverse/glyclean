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
#' @param fasta Either a file path to a FASTA file, a named character vector
#'   with protein IDs as names and sequences as value, or `NULL` to fetch from UniProt.
#' @param n_aa The number of amino acids to the left and right of the glycosylation site.
#' For example, if `n_aa = 5`, the resulting sequence will contain 11 amino acids.
#' @param taxid NCBI taxonomy ID for UniProt lookup. Default: `9606` (human).
#'
#' @returns A [glyexp::experiment()] object with the new "site_sequence" column.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom rlang .data
add_site_seq <- function(exp, fasta = NULL, n_aa = 7, taxid = 9606) {
  UseMethod("add_site_seq")
}

#' @rdname add_site_seq
#' @export
add_site_seq.glyexp_experiment <- function(exp, fasta = NULL, n_aa = 7, taxid = 9606) {
  .add_site_seq_experiment(exp, fasta = fasta, n_aa = n_aa, taxid = taxid)
}

#' @rdname add_site_seq
#' @export
add_site_seq.default <- function(exp, fasta = NULL, n_aa = 7, taxid = 9606) {
  cli::cli_abort(c(
    "{.arg exp} must be a {.cls glyexp_experiment} object.",
    "x" = "Got {.cls {class(exp)}}."
  ))
}

.add_site_seq_experiment <- function(exp, fasta = NULL, n_aa = 7, taxid = 9606) {
  # Check arguments
  checkmate::assert_class(exp, "glyexp_experiment")
  if (glyexp::get_exp_type(exp) != "glycoproteomics") {
    cli::cli_abort(c(
      "The experiment type must be {.val glycoproteomics}.",
      "x" = "Got {.val {glyexp::get_exp_type(exp)}}."
    ))
  }

  # Handle fasta: NULL -> fetch from UniProt
  if (is.null(fasta)) {
    cli::cli_alert_info("Fetching protein sequences from UniProt (taxid: {taxid})")
    unique_proteins <- exp$var_info$protein %>%
      unique() %>%
      purrr::discard(is.na)
    protein_seqs <- .fetch_uniprot_sequences(unique_proteins, taxid)
  } else {
    is_file <- checkmate::test_file_exists(fasta)
    is_named_char <- is.character(fasta) && !is.null(names(fasta))
    if (!is_file && !is_named_char) {
      cli::cli_abort("{.arg fasta} must be a file path, a named character vector, or NULL.")
    }

    if (is_file) {
      protein_seqs <- .read_fasta_file(fasta)
    } else {
      protein_seqs <- .validate_protein_seqs(fasta)
    }
  }

  checkmate::assert_int(n_aa, lower = 1)

  # Check if required columns exist
  if (!"protein" %in% colnames(exp$var_info)) {
    cli::cli_abort("The {.field protein} column does not exist.")
  }

  if (!"protein_site" %in% colnames(exp$var_info)) {
    cli::cli_abort("The {.field protein_site} column does not exist.")
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
  
  # Determine input type for messages
  input_type <- if (is.null(fasta)) {
    "UniProt"
  } else if (is_file) {
    "FASTA file"
  } else {
    "Provided"
  }

  cli::cli_alert_info("{.val {input_type}} contains {.val {length(protein_seqs)}} protein sequences")
  cli::cli_alert_info("Found {.val {length(found_proteins)}} / {.val {length(unique_proteins)}} proteins from experiment")

  if (length(missing_proteins) > 0) {
    # Format missing proteins display
    missing_display <- if (length(missing_proteins) <= 5) {
      stringr::str_c(missing_proteins, collapse = ", ")
    } else {
      stringr::str_c(c(missing_proteins[1:5], "..."), collapse = ", ")
    }

    # Format message based on number of missing proteins
    if (length(missing_proteins) <= 5) {
      cli::cli_alert_warning("Missing proteins: {.val {missing_display}}")
    } else {
      cli::cli_alert_warning("Missing {.val {length(missing_proteins)}} proteins (showing first 5): {.val {missing_display}}")
    }
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

#' Parse UniProt search results into a named sequence vector
#'
#' @param result A data.frame returned by [UniProt.ws::queryUniProt()].
#' @return A named character vector of protein sequences.
#' @keywords internal
#' @noRd
.uniprot_result_to_seqs <- function(result) {
  checkmate::assert_data_frame(result)

  seq_col <- names(result)[stringr::str_to_lower(names(result)) == "sequence"]
  if (length(seq_col) == 0) {
    cli::cli_abort("UniProt response is missing the {.field Sequence} column.")
  }
  seq_col <- seq_col[[1]]

  acc_candidates <- c("Entry", "Accession", "Primary", "Primary Accession")
  acc_col <- acc_candidates[acc_candidates %in% names(result)][1]
  if (is.na(acc_col)) {
    lower_names <- stringr::str_to_lower(names(result))
    acc_col <- names(result)[lower_names %in% c("entry", "accession", "primary", "primary accession")][1]
  }
  if (is.na(acc_col)) {
    cli::cli_abort("UniProt response is missing an accession column.")
  }

  accessions <- stringr::str_trim(as.character(result[[acc_col]]))
  seqs <- as.character(result[[seq_col]])

  seqs <- stats::setNames(seqs, accessions)
  seqs <- seqs[!is.na(names(seqs)) & nzchar(names(seqs))]
  seqs
}

#' Query UniProt with a thin wrapper for easier mocking in tests
#'
#' @param query A UniProt query string.
#' @param fields A character vector of requested fields.
#' @return A data.frame returned by [UniProt.ws::queryUniProt()].
#' @keywords internal
#' @noRd
.query_uniprot <- function(query, fields) {
  UniProt.ws::queryUniProt(query = query, fields = fields)
}

#' Fetch protein sequences from UniProt in batches
#' @keywords internal
.fetch_uniprot_sequences <- function(proteins, taxid = 9606, batch_size = 50) {
  if (length(proteins) == 0) {
    return(character(0))
  }

  # Calculate safe batch size based on estimated query length
  # Each protein ID is ~6 chars, plus " OR " (4 chars) = ~10 chars per protein
  # Leave room for query prefix (~30 chars) and URL overhead
  safe_batch_size <- batch_size

  n <- length(proteins)
  batches <- split(proteins, ceiling(seq_along(proteins) / safe_batch_size))

  cli::cli_alert_info("Fetching {n} proteins in {length(batches)} batch(es)...")

  results <- purrr::map(batches, function(batch_proteins) {
    query_terms <- stringr::str_c("accession:", batch_proteins)
    query_str <- stringr::str_c(query_terms, collapse = " OR ")
    full_query <- paste0("organism_id:", taxid, " AND (", query_str, ")")

    result <- .query_uniprot(
      query = full_query,
      fields = c("accession", "sequence")
    )

    .uniprot_result_to_seqs(result)
  })

  seqs <- unlist(unname(results), recursive = FALSE, use.names = TRUE)
}
