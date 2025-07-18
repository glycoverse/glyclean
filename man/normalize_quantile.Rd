% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_quantile}
\alias{normalize_quantile}
\title{Quantile Normalization}
\usage{
normalize_quantile(x, by = NULL, ...)
}
\arguments{
\item{x}{Either a \code{glyexp_experiment} object or a matrix.
If a matrix, rows should be variables and columns should be samples.}

\item{by}{Either a column name in \code{sample_info} (string) or a factor/vector
specifying group assignments for each sample. Optional.
If provided, the normalization will be performed within each group.}

\item{...}{Additional arguments to pass to \code{\link[limma:normalizequantiles]{limma::normalizeQuantiles()}}.}
}
\value{
Returns the same type as the input. If \code{x} is a \code{glyexp_experiment},
returns a \code{glyexp_experiment} with normalized expression matrix.
If \code{x} is a matrix, returns a normalized matrix.
}
\description{
This function is a wrapper around \code{\link[limma:normalizequantiles]{limma::normalizeQuantiles()}}.
It normalizes the expression matrix by quantile normalization.
This method is used to remove technical variation between samples.
Proteomics data rarely uses this method, but it is common in microarray data.
See \href{https://en.wikipedia.org/wiki/Quantile_normalization}{wikipedia}
for more information.
}
