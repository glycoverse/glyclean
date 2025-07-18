% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_loesscyc}
\alias{normalize_loesscyc}
\title{LoessCyc Normalization}
\usage{
normalize_loesscyc(x, by = NULL, ...)
}
\arguments{
\item{x}{Either a \code{glyexp_experiment} object or a matrix.
If a matrix, rows should be variables and columns should be samples.}

\item{by}{Either a column name in \code{sample_info} (string) or a factor/vector
specifying group assignments for each sample. Optional.
If provided, the normalization will be performed within each group.}

\item{...}{Additional arguments to pass to \code{\link[limma:normalizeCyclicLoess]{limma::normalizeCyclicLoess()}}.}
}
\value{
Returns the same type as the input. If \code{x} is a \code{glyexp_experiment},
returns a \code{glyexp_experiment} with normalized expression matrix.
If \code{x} is a matrix, returns a normalized matrix.
}
\description{
This function is a wrapper around \code{\link[limma:normalizeCyclicLoess]{limma::normalizeCyclicLoess()}} with
\code{method = "pairs"}.
Each pair of columns is normalized mutually to each other.
See \href{https://doi.org/10.1093/bib/bbw095}{this paper} for more information.
Also see \code{\link[limma:normalizeCyclicLoess]{limma::normalizeCyclicLoess()}}.
}
