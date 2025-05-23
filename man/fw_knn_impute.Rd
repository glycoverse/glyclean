% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{fw_knn_impute}
\alias{fw_knn_impute}
\title{Feature-wise KNN Imputation}
\usage{
fw_knn_impute(exp, k = 5, by = NULL, ...)
}
\arguments{
\item{exp}{An expression matrix.}

\item{k}{The number of nearest neighbors to consider.}

\item{by}{A grouping variable to consider when imputing missing values.
This variable should be a column in the variable information table.}

\item{...}{Additional arguments to pass to \code{impute::impute.knn()}.}
}
\value{
An expression matrix with missing values imputed.
}
\description{
A wrapper around the \code{\link[impute:impute.knn]{impute::impute.knn()}}.
Impute missing values with values from the k-nearest neighbors of the
corresponding feature.
}
