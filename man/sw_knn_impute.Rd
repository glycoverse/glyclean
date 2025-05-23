% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{sw_knn_impute}
\alias{sw_knn_impute}
\title{Sample-wise KNN Imputation}
\usage{
sw_knn_impute(exp, k = 5, by = NULL, ...)
}
\arguments{
\item{exp}{An expression matrix.}

\item{k}{The number of nearest neighbors to consider.}

\item{by}{A grouping variable to consider when imputing missing values.
This variable should be a column in the sample information table.}

\item{...}{Additional arguments to pass to \code{impute::impute.knn()}.}
}
\value{
An expression matrix with missing values imputed.
}
\description{
A wrapper around the \code{\link[impute:impute.knn]{impute::impute.knn()}}.
Impute missing values with values from the k-nearest neighbors of the
corresponding sample.
If there are strong patterns among the samples (such as group clustering
relationships or experimental conditions), this method can better utilize
the overall relationships among samples.
}
