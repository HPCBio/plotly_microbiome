\name{beta_diversity_3d}
\alias{beta_diversity_3d}
\alias{beta_diversity_3d.data.frame}
\alias{beta_diversity_3d.monoMDS}
\title{
Draw 3D Scatterplot of Beta Diversity
}
\description{
This is a wrapper function for \code{plot_ly} with the
"scatterplot3d" option.  It can be used on a data frame, or directly on the
output of \code{phyloseq::ordinate} with the NMDS method or \code{vegan::metaMDS}.
Default categorical colors are from \pkg{dittoSeq}.
}
\usage{
beta_diversity_3d(x, ...)

\method{beta_diversity_3d}{data.frame}(x, axes = colnames(x)[1:3],
                  color.column = colnames(x)[4],
                  label.column = colnames(x)[5],
                  color.key = NULL, \dots)

\method{beta_diversity_3d}{monoMDS}(x, metadata,
                  color.column,
                  label.column = NULL,
                  color.key = NULL, \dots)
}

\arguments{
  \item{x}{
A data frame or \code{"monoMDS"} object containing ordination results.
}
\item{axes}{
If \code{x} is a data frame, the column names for three axes to plot.
}
\item{color.column}{
The name of a numeric or categorical column to be used for coloring points.
This column should be found in \code{x} if \code{x} is a data frame, or in
\code{metadata} otherwise.
}
\item{label.column}{
The name of a character column to be used for labeling points.
This column should be found in \code{x} if \code{x} is a data frame, or in
\code{metadata} otherwise.  It defaults to sample names if \code{x} is a
\code{"monoMDS"}.
}
\item{color.key}{
If \code{color.column} refers to a character or factor column, a named vector
of colors, with names corresponding to values in the column.  The default is
to use \code{dittoSeq::dittoColors}.
If \code{color.column} refers to a numeric column, a long vector of colors
to be used as the color scale.  The default is to use \code{viridis}.
Passed to the \code{colors} argument of \code{plot_ly}.
}
\item{metadata}{
A data frame of sample metadata, in the same order as \code{x}.
}
  \item{\dots}{
Optional arguments passed to \code{plot_ly}.
}
}

\value{
A \code{"plotly"} object.
}

\author{
Lindsay Clark
}

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
\dontrun{
# Perform NMDS with Bray distance

ord_mfiber_prop1a <- ordinate(ps_mfiber_prop, "NMDS", "bray", k = 3)

beta_diversity_3d(ord_mfiber_prop1a,
                  sample_data(ps_mfiber_prop), "TRT", "Label")

# NMDS with UniFrac distance (ordinate won't let you adjust k)

dist_mfiber_4 <- phyloseq::distance(ps_mfiber_prop, "unifrac")
ord_mfiber_prop4a <- vegan::metaMDS(dist_mfiber_4, k = 3)

beta_diversity_3d(ord_mfiber_prop4a,
                  sample_data(ps_mfiber_prop), "TRT", "Label")
}
}
