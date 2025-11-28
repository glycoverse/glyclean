# glyclean: Perform Preprocessing on Glycomics and Glycoproteomics Data

Provides comprehensive data preprocessing tools for glycomics and
glycoproteomics experiments. The package offers both automated and
manual preprocessing pipelines, including normalization (median,
quantile, vsn, total area), missing value handling (filtering,
imputation using various methods), batch effect correction (using ComBat
and other methods), data aggregation (at peptide, glycan, or site
level), and quality control functions. The automated pipeline
intelligently selects appropriate methods based on data characteristics
and sample size, making it easy to prepare clean, analysis-ready data
for downstream statistical analysis within the glycoverse ecosystem.
Works seamlessly with 'glyexp::experiment()' objects to ensure data
consistency and interoperability.

## See also

Useful links:

- <https://glycoverse.github.io/glyclean/>

## Author

**Maintainer**: Bin Fu <23110220018@m.fudan.edu.cn>
([ORCID](https://orcid.org/0000-0001-8567-2997))
