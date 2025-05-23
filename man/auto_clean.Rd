% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto-clean.R
\name{auto_clean}
\alias{auto_clean}
\title{Automatic Data Preprocessing}
\usage{
auto_clean(exp, to_level = c("gf", "gp", "gfs", "gps"))
}
\arguments{
\item{exp}{A \code{glyexp_experiment} object containing glycoproteomics data.}

\item{to_level}{The aggregation level. Default is "gf" (glycoform).
See \code{\link[=aggregate]{aggregate()}} for details.}
}
\value{
A modified \code{glyexp_experiment} object
}
\description{
Perform automatic data preprocessing on glycoproteomics data.
Calling r\code{auto_clean(exp)} equals to:
}
\details{
\if{html}{\out{<div class="sourceCode r">}}\preformatted{exp |>
  remove_missing_variables(prop = 0.5) |>
  median_normalize() |>
  zero_impute() |>
  aggregate(to_level = "gf")
}\if{html}{\out{</div>}}
}
