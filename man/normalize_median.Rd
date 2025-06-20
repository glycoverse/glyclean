% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize_median}
\alias{normalize_median}
\title{Median Normalization}
\usage{
normalize_median(exp)
}
\arguments{
\item{exp}{An experiment object.}
}
\value{
An experiment object with the expression matrix normalized.
}
\description{
Normalize the expression matrix by dividing each column (sample) by the median
of that column (NA ignored), so that the median of each column is 1.
This is the most common normalization method for proteomics data.
It effectively and robustly removes the bias introduced by total protein abundance,
and removes batch effects in part.
}
