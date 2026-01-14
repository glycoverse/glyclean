# aggregating from glycoforms to glycopeptides fails

    Code
      aggregate(exp, to_level = "gp", standardize_variable = FALSE)
    Condition
      Error in `glyclean_aggregate()`:
      ! All required columns must be present in `var_info`.
      i Required columns: peptide, protein, glycan_composition, peptide_site, and protein_site.
      x Missing columns: peptide and peptide_site.

# aggregating from glycoforms without structures to glycoforms with structures fails

    Code
      aggregate(exp, to_level = "gfs", standardize_variable = FALSE)
    Condition
      Error in `glyclean_aggregate()`:
      ! All required columns must be present in `var_info`.
      i Required columns: protein, glycan_composition, glycan_structure, and protein_site.
      x Missing columns: glycan_structure.
      i You might want to aggregate to "gp" or "gf" level.

