\name{composition_df}
\alias{composition_df}
\title{
Prepare a Data Frame for Composition Plots
}
\description{
This function takes a \code{phyloseq} object and prepares a data frame that
can be used to generate composition plots with \pkg{ggplot2}. Works similarly
to the \code{ggformat} function in \pkg{phyloseq.extended}.
}
\usage{
composition_df(psobj, rank = "Family",
               keepcols = c("Sample", "Group", "Label", "ID"),
               minprop = 0.05, mean_across_samples = NULL)
}
\arguments{
  \item{psobj}{
A \code{phyloseq} object containing raw, potentially agglomerated, counts.
}
  \item{rank}{
The taxonomic rank across which taxa counts shoud be summed.
}
  \item{keepcols}{
Names of columns from \code{sample_data(psobj)}
}
  \item{minprop}{
Threshold for showing a taxon vs. lumping it into "Other".  At least one
sample must have at least this proportion of the OTU counts assigned to a given
taxon for that taxon to be displayed.
}
  \item{mean_across_samples}{
An optional grouping variable, indicated as a character string matching one of
the column names in \code{keepcols}.  If provided, samples are lumped within
groups.  Proportions are averaged across samples, and counts are summed across
samples.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A tibble with the following columns:

\itemize{
  \item A column labeled \code{ID} listing sample names, or if provided, a 
  column named the same as \code{mean_across_samples}.
  \item A column named the same as \code{rank} listing taxa names.
  \item A column named \code{Proportion} indicating the proportion of OTU counts
  assigned to a given taxon within a sample or group.
  \item A column named \code{Counts} indicating the total OTU counts assigned
  to a given taxon within a sample or group.
  \item Any other columns in \code{keepcols}. If \code{mean_across_samples} is
  provided, columns are dropped if they have more than one value for a given
  group.
}
}

\author{
Lindsay Clark
}

\seealso{
\code{\link{findnonmissing}} is used to determine which taxa to label as
\dQuote{Unclassified}.
}
\examples{
\dontrun{
# Columns to keep
kc <- c("Dog", "Trt", "Day", "Breed", "Group", "Label", "ID")

# Composition plot on individuals, grouped by experimental group
p1 <- composition_df(ps_glom, "Family", minprop = 0.1,
               keepcols = kc) %>%
  ggplot(aes(x = ID, y = Proportion, fill = Family)) +
  geom_col() +
  facet_wrap(~ Group, scales = "free_x") +
  scale_fill_manual(values = dittoSeq::dittoColors(1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplotly(p1)

# Composition plot on treatments
p2 <- composition_df(ps_glom, "Family", minprop = 0.1,
               keepcols = kc, mean_across_samples = "Trt") %>%
  ggplot(aes(x = Trt, y = Proportion, fill = Family)) +
  geom_col() +
  scale_fill_manual(values = dittoSeq::dittoColors(1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplotly(p2)
}
}
