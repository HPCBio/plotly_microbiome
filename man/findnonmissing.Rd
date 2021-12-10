\name{findnonmissing}
\alias{findnonmissing}
\title{
Identify Non-Missing Taxonomic Labels
}
\description{
In various taxonomic databases and pipelines, unknown or missing
taxonomic labels may be indicated in a variety of ways, such as
missing data, "unclassified", "uncultured", etc.  This function
identifies all of these that I have encountered so far.
}
\usage{
findnonmissing(x)
}
\arguments{
  \item{x}{
A character vector of taxonomic labels, for example a single
column of the \code{tax_table} slot of a \code{phyloseq} object.
}
}
\details{
The following values will result in output of \code{FALSE}.

\itemize{
\item \code{NA}
\item An empty string.
\item The words \dQuote{unclassified}, \dQuote{unidentified},
\dQuote{uncultured}, or \dQuote{metagenome} anywhere in the string,
in any case.
\item Values equal to \dQuote{human_gut.}
}
}
\value{
A logical vector, with \code{TRUE} if the taxonomic label reflects a
taxonomic identity, and \code{FALSE} if it should be considered missing.
}
\author{
Lindsay V. Clark
}

\examples{
findnonmissing(c("Streptococcus", "Blautia", "Horse metagenome", NA))
}

