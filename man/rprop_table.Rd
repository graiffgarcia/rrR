% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrR.R
\name{rprop_table}
\alias{rprop_table}
\title{Express a prop_table in percentages}
\usage{
rprop_table(..., margin = 0, digits = 1)
}
\arguments{
\item{...}{arguments to be passed to methods.}

\item{margin}{index to generate margin for}

\item{digits}{integer indicating the number of decimal places
    (\code{round}) or significant digits (\code{signif}) to be used.
    Negative values are allowed (see \sQuote{Details}).}
}
\description{
A shortcut for prop_table that displays values as percentages and 
rounds them. For example, rprop_table(var, 1) is equivalent to 
round(prop_table(var)*100, 1).
 
To avoid confusion with the margin argument in prop_table, the digits
argument here must be named. to allow for piping, in a way similar to 
wtable(), the margin argument here must be named as well.
}
