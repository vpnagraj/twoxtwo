% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{summary.twoxtwo}
\alias{summary.twoxtwo}
\title{Summarize twoxtwo object}
\usage{
\method{summary}{twoxtwo}(object, alpha = 0.05, ...)
}
\arguments{
\item{object}{\link[twoxtwo]{twoxtwo} object}

\item{alpha}{Significance level to be used for constructing confidence interval; default is \code{0.05}}

\item{...}{Additional arguments passed to \link[twoxtwo]{print.twoxtwo}}
}
\value{
Printed summary information including the outcome and exposure variables and levels, as well as the number of missing observations, the \code{twoxtwo} contingency table, and formatted effect measures (see "Description"). In addition to printed output, the function invisibly returns a named list with computed effect measures (i.e. the \code{tibble} outputs from \link[twoxtwo]{odds_ratio}, \link[twoxtwo]{risk_ratio}, and \link[twoxtwo]{risk_diff} respectively).
}
\description{
The \code{summary.twoxtwo()} function provides an S3 method for summarizing objects created with \link[twoxtwo]{twoxtwo}. The summary function prints the \code{twoxtwo} via \link[twoxtwo]{print.twoxtwo} along with characteristics of the contingency table such the number of missing observations and exposure/outcome variables and levels. The summary will also compute effect measures using \link[twoxtwo]{odds_ratio}, \link[twoxtwo]{risk_ratio}, and \link[twoxtwo]{risk_diff} and print the estimates and confidence interval for each.
}
