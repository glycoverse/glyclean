# Add site-specific sequence information.

This function adds a new "site_sequence" column to the variable
information table. It is the protein sequence around the glycosylation
site. If the head and tail amino acids of the peptide sequence are
insufficient, fill with "X".

This function requires the following columns in the variable information
tibble:

- "protein": The protein uniprot accession.

- "protein_site": The site on the protein sequence.

## Usage

``` r
add_site_seq(exp, fasta = NULL, n_aa = 7, taxid = 9606)

# S3 method for class 'glyexp_experiment'
add_site_seq(exp, fasta = NULL, n_aa = 7, taxid = 9606)

# Default S3 method
add_site_seq(exp, fasta = NULL, n_aa = 7, taxid = 9606)
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object with "glycoproteomics" type.

- fasta:

  Either a file path to a FASTA file, a named character vector with
  protein IDs as names and sequences as value, or `NULL` to fetch from
  UniProt.

- n_aa:

  The number of amino acids to the left and right of the glycosylation
  site. For example, if `n_aa = 5`, the resulting sequence will contain
  11 amino acids.

- taxid:

  NCBI taxonomy ID for UniProt lookup. Default: `9606` (human).

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with the new "site_sequence" column.
