% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_rlr}
\alias{normalize_rlr}
\title{Robust Linear Regression Normalization}
\usage{
normalize_rlr(x, by = NULL)
}
\arguments{
\item{x}{Either a \code{glyexp_experiment} object or a matrix.
If a matrix, rows should be variables and columns should be samples.}

\item{by}{Either a column name in \code{sample_info} (string) or a factor/vector
specifying group assignments for each sample. Optional.
If provided, the normalization will be performed within each group.}
}
\value{
Returns the same type as the input. If \code{x} is a \code{glyexp_experiment},
returns a \code{glyexp_experiment} with normalized expression matrix.
If \code{x} is a matrix, returns a normalized matrix.
}
\description{
This method is based on robust linear regression. The reference sample is
calculated as the median value of each variable's abundance across all
measured samples. For each sample, a robust linear regression model is
fitted to the sample's abundance values against the reference sample's
abundance values. The fitted model is then used to normalize the sample's
abundance values. The underlying assumption is that the diﬀerent intensities
observed across individuals are imputable to diﬀerent amounts of the biological
material in the collected samples.
}
