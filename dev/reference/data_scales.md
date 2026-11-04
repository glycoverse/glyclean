# Abundance scales in preprocessing and quality control

Glyclean reads `scale` from experiment metadata. An absent field
defaults to `"raw"`; explicitly invalid values are rejected. `"log"` is
the natural logarithm, while `"log2"` and `"log10"` use their named
bases. `"logx"` denotes log-like coordinates whose original abundances
cannot be recovered from scale metadata alone. Ordinary processing
converts to the algorithm's working domain and restores the input scale.
Filters return subsets of the original assay without a numeric round
trip. These conventions apply to legacy experiments, glyco subclasses,
and plain `SummarizedExperiment` objects wherever the operation supports
them.

## Working domains

- Raw abundance: total-area, median, absolute-median, median-quotient
  and quantile normalization; MissForest and half-sample-minimum
  imputation; aggregation (including custom `f`); low-expression,
  variance and CV filters; total-intensity and CV plots. Custom
  aggregation callbacks receive raw values after missing values are
  removed. A sum over an all-missing group is zero, represented as
  `-Inf` on ordinary log scales.

- Log2 abundance: loess and robust-regression normalization; KNN, BPCA,
  PPCA, SVD and MinProb imputation; rank abundance, intensity boxplots,
  RLE, PCA and replicate scatter plots. RLE subtracts the row median in
  this working domain.

Batch detection and correction both use `log2(raw + 1e-6)`. Correction
reverses this offset and truncates negative corrected abundances to zero
(`-Inf` on ordinary log scales). Skipped or failed corrections keep the
original assay exactly.

Missingness and constancy are evaluated in the current representation.
Sample-minimum imputation also uses the current representation; wholly
missing samples remain missing. Half-minimum imputation likewise leaves
wholly missing samples missing.

## Log-like coordinates

`"logx"` denotes log-like coordinates whose original abundance cannot be
recovered from the scale alone. Supported algorithms use these
coordinates directly, including legitimate negative values and zeros. No
logarithm, exponentiation, pseudocount, or automatic CLR recentering is
applied.

`logx` supports quantile, loess and robust-regression normalization;
KNN, BPCA, PPCA, SVD, MissForest and sample-minimum imputation;
missingness and constant-variable filters; batch detection/correction;
missingness, intensity, RLE, PCA and replicate plots; and annotation.
Suitability of each model for particular CoDA or VSN coordinates still
depends on the data.

Operations requiring recoverable abundances reject `logx`: total-area,
median, absolute-median and median-quotient normalization; half-minimum
and MinProb imputation; low-CV, variance and expression filters;
aggregation; TIC, CV and rank-abundance plots; and repeated CoDA or VSN
transformations.
[`impute_zero()`](https://glycoverse.github.io/glyclean/dev/reference/impute_zero.md)
accepts only `raw`.
[`zero_to_na()`](https://glycoverse.github.io/glyclean/dev/reference/zero_to_na.md)
replaces zero on `raw` and `-Inf` on ordinary log scales, preserving
finite coordinate zero and other non-finite values; it rejects `logx`.

## Transformations and automatic processing

[`transform_log()`](https://glycoverse.github.io/glyclean/dev/reference/transform_log.md)
explicitly converts ordinary abundance scales to `log`, `log2`, or
`log10`, updating metadata without adding a pseudocount. It is generally
unnecessary for downstream Glycoverse analyses and plots that already
transform their inputs; see its documentation for input contracts.

[`transform_clr()`](https://glycoverse.github.io/glyclean/dev/reference/transform_clr.md),
[`transform_alr()`](https://glycoverse.github.io/glyclean/dev/reference/transform_alr.md),
[`auto_coda()`](https://glycoverse.github.io/glyclean/dev/reference/auto_coda.md)
and
[`normalize_vsn()`](https://glycoverse.github.io/glyclean/dev/reference/normalize_vsn.md)
accept ordinary scales and deliberately produce `scale = "logx"`. CoDA
returns log2-ratio coordinates and VSN returns its log-like coordinates.
CoDA requires finite, positive abundances (finite ordinary log input),
with no missing values. Other log-domain algorithms require finite
observed coordinates; use
[`zero_to_na()`](https://glycoverse.github.io/glyclean/dev/reference/zero_to_na.md)
and suitable imputation or filtering first.

Automatic functions keep their existing algorithm selection and inherit
that algorithm's scale policy.
[`auto_clean()`](https://glycoverse.github.io/glyclean/dev/reference/auto_clean.md)
and
[`auto_normalize()`](https://glycoverse.github.io/glyclean/dev/reference/auto_normalize.md)
reject `logx`;
[`auto_impute()`](https://glycoverse.github.io/glyclean/dev/reference/auto_impute.md)
rejects it when its selected algorithm is MinProb. Pipelines store the
actual processing function and parameters and perform no hidden scale
conversion. ALR fallback records CLR, restarting from the supplied seed
so its history faithfully replays the fallback transformation.

Protein adjustment has a separate abundance contract; see
[`adjust_protein()`](https://glycoverse.github.io/glyclean/dev/reference/adjust_protein.md).
Its scale API is unchanged by this adaptation.
