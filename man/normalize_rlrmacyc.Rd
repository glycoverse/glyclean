% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_rlrmacyc}
\alias{normalize_rlrmacyc}
\title{Robust Linear Regression with Median Adjustment and Cyclic Normalization}
\usage{
normalize_rlrmacyc(x, n_iter = 3, by = NULL)
}
\arguments{
\item{x}{Either a \code{glyexp_experiment} object or a matrix.
If a matrix, rows should be variables and columns should be samples.}

\item{n_iter}{The number of iterations to perform. Default is 3.}

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
This method is based on robust linear regression with median adjustment
and cyclic normalization. The method is applied iteratively to each pair
of samples. For each pair of samples, the median of the differences between
the two samples is calculated. A robust linear regression model is fitted
to the differences against the averages of the two samples. The fitted model
is then used to normalize the two samples. The process is repeated for a
number of iterations.
}
