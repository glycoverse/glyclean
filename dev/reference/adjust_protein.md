# Adjust Protein Expression

This function adjusts the glycopeptide expression by the protein
expression. In another word, it "strips out" the protein expression from
the glycopeptide expression, so that the expression reflects the
glycosylation status only.

## Usage

``` r
adjust_protein(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
)

# S3 method for class 'glyexp_experiment'
adjust_protein(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
)

# Default S3 method
adjust_protein(
  exp,
  pro_expr_mat,
  method = c("ratio", "reg"),
  pro_scale = "raw"
)
```

## Arguments

- exp:

  A
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  object.

- pro_expr_mat:

  A matrix of protein expression. Columns are named samples, rows are
  named UniProt protein accessions that match the `protein` annotation
  in `exp`.

- method:

  The method to use for protein adjustment. Either "ratio" or "reg".
  Default is "ratio".

- pro_scale:

  Representation of `pro_expr_mat`, independently of the scale metadata
  of `exp`: `"raw"` (default), `"log"`, `"log2"`, or `"log10"`.

## Value

A
[`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
object with adjusted protein expression, represented on the input scale
of `exp`.

## Details

For simplicity, glycopeptide expression is denoted as `GP`, and protein
expression is denoted as `PRO`.

### "ratio" method

`GP-adj` = (`GP` / `PRO`) / (median(`GP`) / median(`PRO`))

The first part is to adjust glycopeptide expression by protein
expression. The second part normalizes the ratios by the ratio of the
row medians. The result is a normalized ratio, not an absolute
abundance.

### "reg" method

Use linear regression to remove the protein expression from the
glycopeptide expression. Both glycopeptide and protein expression values
are log2-transformed (with +1 to avoid log(0)) before fitting the linear
model: log2(GP+1) ~ log2(PRO+1). The residuals represent the
glycosylation-specific signal and are converted back to the abundance
domain using 2^residuals, giving positive exponentiated residuals. Rows
with fewer than three paired observations or failed fits retain their
original glycopeptide values.

In both methods, only glycoproteins and samples identified in both `exp`
and `pro_expr_mat` are retained, with samples aligned by name.

Both inputs may independently use ordinary `raw`, `log` (natural
logarithm), `log2`, or `log10` representations. Logarithms are decoded
to raw abundances before applying the formulas above; these are ordinary
logarithms, not `log2(x + 1)` working values. Results use the original
representation and scale metadata of `exp`. The `logx` label is rejected
for either input because it does not identify an invertible abundance
transformation.

Abundances must be nonnegative and finite or missing. Negative log
values represent abundances below one and are allowed; `-Inf` represents
raw zero. Log values that overflow or underflow when decoded to raw are
rejected. The ratio method preserves ordinary division behavior at zero
denominators or zero medians, including non-finite results; it does not
impute zeros.

## Examples

``` r
exp <- glyexp::as_glycoproteomic_se(glyexp::real_experiment)
proteins <- unique(SummarizedExperiment::rowData(exp)$protein)
pro <- matrix(1, length(proteins), ncol(exp),
  dimnames = list(proteins, colnames(exp)))
adjusted <- adjust_protein(exp, log2(pro), pro_scale = "log2")
```
