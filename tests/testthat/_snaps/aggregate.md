# aggregation levels must match the experiment type

    Code
      aggregate(aggregation_glycomic_se(), to_level = "gf", standardize_variable = FALSE)
    Condition
      Error in `glyclean_aggregate()`:
      ! The aggregation level must match the experiment type.
      i "glycomics" data supports "g" and "gs".
      x Got "gf".

---

    Code
      aggregate(complex_exp(), to_level = "g", standardize_variable = FALSE)
    Condition
      Error in `glyclean_aggregate()`:
      ! The aggregation level must match the experiment type.
      i "glycoproteomics" data supports "gf", "gp", "gfs", and "gps".
      x Got "g".

# glycomics structure aggregation requires glycan structures

    Code
      aggregate(aggregation_glycomic_se(include_structure = FALSE), to_level = "gs",
      standardize_variable = FALSE)
    Condition
      Error in `glyclean_aggregate()`:
      ! All required columns must be present in `var_info`.
      i Required columns: glycan_composition and glycan_structure.
      x Missing columns: glycan_structure.
      i You might want to aggregate to "g" level.

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

