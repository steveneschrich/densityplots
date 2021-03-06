% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dv_annotate_count_axis.R
\name{dv_annotate_count_axis}
\alias{dv_annotate_count_axis}
\title{Annotate counts axis}
\usage{
dv_annotate_count_axis(
  counts,
  gp = grid::gpar(),
  prefix = "N",
  sep = "=",
  title = "N",
  vp = NULL
)
}
\arguments{
\item{counts}{The counts to annotate plot with}

\item{gp}{Graphical parameters for labels (see \code{\link[grid:gpar]{grid::gpar()}}).}

\item{prefix}{A prefix for label counts (e.g., N)}

\item{sep}{A separator between prefix and count (e.g., =)}

\item{title}{The title to use (default is N)}

\item{vp}{Viewport to plot in (see \code{\link[grid:viewport]{grid::viewport()}})}
}
\value{
A grob representing an annotated axis.
}
\description{
Annotate counts axis
}
\examples{
\dontrun{
dv_annotate_count_axis(counts=c(2,5,1,2), prefix="N", sep=":", title="Counts", vp = "panel")
}
}
