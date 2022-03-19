\name{make_taxa_labels}
\alias{make_taxa_labels}
\title{
Create a Label for Each Taxon
}
\description{
This function takes a table of taxonomic ranks, such as that stored in the
\code{tax_table} slot of a \code{phyloseq} object, and creates a label for each
taxon for use in plots and tables.
}
\usage{
make_taxa_labels(taxtab)
}
\arguments{
  \item{taxtab}{
A matrix or data frame, with taxa in rows and taxonoic ranks in columns.
The last column should be \dQuote{Species}, the first column should be kingdom
or domain, and columns in between should progress in order of rank.
}
}
\details{
Species labels that pass \code{\link{findnonmissing}} are used, and
otherwise species are labeled \dQuote{"sp."}. The lowest rank that passes
\code{findnonmissing} is pasted before the species label.
}
\value{
A character vector containing the labels. If \code{taxtab} has row names,
these are used to name the vector.
}
\author{
Lindsay V. Clark
}
\examples{
tt <- matrix(c("Bacteria", "Firmicutes", "Clostridia",
               "Peptostreptococcales-Tissierellales", "Anaerovoracaceae",
               "Mogibacterium", "Unclassified",
               "Bacteria", "Firmicutes", "Clostridia", "Oscillospirales",
               "Ruminococcaceae", "Faecalibacterium", "prausnitzii",
               "Bacteria", "Firmicutes", "Clostridia",
               "Clostridia vadinBB60 group", "Unclassified", "Unclassified",
               "Unclassified"),
               nrow = 3, ncol = 7, byrow = TRUE,
               dimnames = list(c("db3b201de3a824d7356ce4d8360e5bc3",
                                 "615ce01e68e098adc445d06e072ba255",
                                 "6f643e7faeab3e737b38f5304b841d97"),
                               c("Kingdom", "Phylum", "Class", "Order",
                                 "Family", "Genus", "Species")))

make_taxa_labels(tt)
}
