# Aggregate Data

Aggregate glycoproteomics data to different levels (glycoforms,
glycopeptides, etc.). This function sums up quantitative values for each
unique combination of specified variables. It is recommended to call
this function after missing value imputation.

The following levels are available:

- "gf": Aggregate to glycoforms, which is the unique combination of
  proteins, protein sites, and glycan compositions. This is the default
  level.

- "gp": Aggregate to glycopeptides, which is the unique combination of
  peptides, proteins, protein sites, and glycan compositions.

- "gfs": Like "gf", but differentiates structures with the same
  composition.

- "gps": Like "gp", but differentiates structures with the same
  composition.

Different levels of aggregation require different columns in the
variable information.

- "gf": "protein", "glycan_composition", "protein_site"

- "gp": "peptide", "protein", "glycan_composition", "peptide_site",
  "protein_site"

- "gfs": "protein", "glycan_composition", "glycan_structure",
  "protein_site"

- "gps": "peptide", "protein", "glycan_composition", "glycan_structure",
  "peptide_site", "protein_site"

Other columns in the variable information tibble with "many-to-one"
relationship with the unique combination of columns listed above will be
kept. A common example is the "gene" column. For each "glycoform"
(unique combination of "protein", "protein_site", and
"glycan_composition"), there should be only one "gene" value, therefore
it is kept for "gf" level. On the other hand, the "peptide" column is
removed for "gf" level, as one "glycoform" can contain multiple
"peptides".

## Usage

``` r
aggregate(exp, to_level = c("gf", "gp", "gfs", "gps"))

# S3 method for class 'glyexp_experiment'
glyclean_aggregate(exp, to_level = c("gf", "gp", "gfs", "gps"))

# Default S3 method
glyclean_aggregate(exp, to_level = c("gf", "gp", "gfs", "gps"))
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object with "glycoproteomics" type.

- to_level:

  The aggregation level, one of: "gf" (glycoforms), "gp"
  (glycopeptides), "gfs" (glycoforms with structures), or "gps"
  (glycopeptides with structures). See Details for more information.

## Value

A modified
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with aggregated expression matrix and updated variable
information.
