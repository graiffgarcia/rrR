% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrR.R
\name{prop_table}
\alias{prop_table}
\title{A version of prop.table that takes variables directly (like table).}
\usage{
prop_table(
  ...,
  margin = 0,
  exclude = if (useNA == "no") c(NA, NaN),
  useNA = c("no", "ifany", "always"),
  dnn = list.names(...),
  deparse.level = 1
)
}
\arguments{
\item{...}{one or more objects which can be parsed by the base table function}

\item{margin}{index to generate margin for}

\item{exclude}{levels to remove for all factors in \code{\dots}.  If
    it does not contain \code{\link[base]{NA}} and \code{useNA} is not
    specified, it implies \code{useNA = "ifany"}.  See
    \sQuote{Details} for its interpretation for non-factor arguments.}

\item{useNA}{whether to include \code{NA} values in the table.
    See \sQuote{Details}.  Can be abbreviated.}

\item{dnn}{the names to be given to the dimensions in the result (the
    \emph{dimnames names}).}

\item{deparse.level}{controls how the default \code{dnn} is
    constructed.  See \sQuote{Details}.}
}
\description{
A version of prop.table that takes any R object that the 'table' function
will accept. This, like object_size, is a convenience function so that you
don't have to always type prop.table(table(object)). This function allows you
to specify the margin of the table as the LAST unnamed argument, meaning that
the function works if every argument you pass *after* margin is named. Of
course, you can explicitly pass 'margin = n' and then everything should work
as expected. For now, if you pass margin as an unnamed argument, it
cannot be a vector (unlike in prop.table).
}
