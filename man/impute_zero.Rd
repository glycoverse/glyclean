% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{impute_zero}
\alias{impute_zero}
\title{Zero Imputation}
\usage{
impute_zero(x)
}
\arguments{
\item{x}{Either a \code{glyexp_experiment} object or a matrix.
If a matrix, rows should be variables and columns should be samples.}
}
\value{
Returns the same type as the input. If \code{x} is a \code{glyexp_experiment},
returns a \code{glyexp_experiment} with missing values imputed.
If \code{x} is a matrix, returns a matrix with missing values imputed.
}
\description{
Impute missing values in an expression matrix by replacing them with zeros.
}
