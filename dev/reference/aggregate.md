# Aggregate Data

Aggregate glycomics or glycoproteomics data to different levels
(glycans, glycoforms, glycopeptides, etc.). This function sums up
quantitative values for each unique combination of specified variables.
It is recommended to call this function after missing value imputation.

The following levels are available:

- "g": Aggregate glycomics data to glycan compositions.

- "gs": Aggregate glycomics data to glycan structures.

- "gf": Aggregate to glycoforms, which is the unique combination of
  proteins, protein sites, and glycan compositions.

- "gp": Aggregate to glycopeptides, which is the unique combination of
  peptides, proteins, protein sites, and glycan compositions.

- "gfs": Like "gf", but differentiates structures with the same
  composition.

- "gps": Like "gp", but differentiates structures with the same
  composition.

The "g" and "gs" levels are available for glycomics data. The "gf",
"gp", "gfs", and "gps" levels are available for glycoproteomics data.
When `to_level = NULL`, glycomics data defaults to "gs" when
`glycan_structure` is present and "g" otherwise. Glycoproteomics data
similarly defaults to "gfs" or "gf".

Different levels of aggregation require different columns in the
variable information.

- "g": "glycan_composition"

- "gs": "glycan_composition", "glycan_structure"

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
aggregate(exp, to_level = NULL, standardize_variable = TRUE)

# S3 method for class 'glyexp_experiment'
glyclean_aggregate(exp, to_level = NULL, standardize_variable = TRUE)

# Default S3 method
glyclean_aggregate(exp, to_level = NULL, standardize_variable = TRUE)
```

## Arguments

- exp:

  A glycomics or glycoproteomics container: a
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html),
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html),
  or legacy `glyexp_experiment` object.

- to_level:

  The aggregation level. If `NULL` (the default), glycomics data uses
  "gs" when `glycan_structure` is present and "g" otherwise;
  glycoproteomics data uses "gfs" when `glycan_structure` is present and
  "gf" otherwise. Explicit values are: "g" (glycan compositions), "gs"
  (glycan structures), "gf" (glycoforms), "gp" (glycopeptides), "gfs"
  (glycoforms with structures), or "gps" (glycopeptides with
  structures). See Details for more information.

- standardize_variable:

  Whether to call
  [`glyexp::standardize_variable()`](https://glycoverse.github.io/glyexp/reference/standardize_variable.html)
  after aggregation. Set to `FALSE` to skip network calls for faster
  testing. Default is `TRUE`.

## Value

A modified container with the same class as `exp`, an aggregated
expression matrix, and updated variable information.
