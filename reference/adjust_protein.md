# Adjust Protein Expression

This function adjusts the glycopeptide expression by the protein
expression. In another word, it "strips out" the protein expression from
the glycopeptide expression, so that the expression reflects the
glycosylation status only.

## Usage

``` r
adjust_protein(exp, pro_expr_mat, method = c("ratio", "reg"))

# S3 method for class 'glyexp_experiment'
adjust_protein(exp, pro_expr_mat, method = c("ratio", "reg"))

# Default S3 method
adjust_protein(exp, pro_expr_mat, method = c("ratio", "reg"))
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object with "glycoproteomics" type.

- pro_expr_mat:

  A matrix of protein expression. Columns are samples, rows are uniprot
  protein accessions.

- method:

  The method to use for protein adjustment. Either "ratio" or "reg".
  Default is "ratio".

## Value

A
[`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
object with adjusted protein expression.

## Details

For simplicity, glycopeptide expression is denoted as `GP`, and protein
expression is denoted as `PRO`.

### "ratio" method

`GP-adj` = (`GP` / `PRO`) / (median(`GP`) / median(`PRO`))

The first part is to adjust glycopeptide expression by protein
expression. The second part is to rescale the expression to the original
scale.

### "reg" method

Use linear regression to remove the protein expression from the
glycopeptide expression. Both glycopeptide and protein expression values
are log2-transformed (with +1 to avoid log(0)) before fitting the linear
model: log2(GP+1) ~ log2(PRO+1). The residuals represent the
glycosylation-specific signal and are converted back to the original
scale using 2^residuals, ensuring all adjusted values are positive.

In both methods, only glycoproteins identified in both `exp` and
`pro_expr_mat` will be retained.
