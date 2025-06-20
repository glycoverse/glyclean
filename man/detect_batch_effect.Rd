% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch-correction.R
\name{detect_batch_effect}
\alias{detect_batch_effect}
\title{Detect batch effect}
\usage{
detect_batch_effect(exp, batch_col = "batch", group_col = NULL)
}
\arguments{
\item{exp}{A \code{glyexp::experiment()}.}

\item{batch_col}{The column name of the batch variable in the sample information.
Default to "batch".}

\item{group_col}{The column name of the group variable in the sample information.
If provided, it will be used as a covariate in the ANOVA model.
This is useful when you have an unbalanced design,
i.e., the proportion of groups in each batch is not the same.}
}
\value{
A double vector of p-values for each variable,
i.e., the same length as \code{nrow(get_expr_mat(exp))}
}
\description{
Use ANOVA to detect if batch effect is present in the data.
If \code{group_col} is provided,
it will be used as a covariate in the ANOVA model.
}
