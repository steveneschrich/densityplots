% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{compute_density_limits}
\alias{compute_density_limits}
\title{Compute density limits}
\usage{
compute_density_limits(x, limits = NULL, bw = "nrd0", cut = 3, trim = TRUE)
}
\arguments{
\item{x}{Numeric vector of points to estimate density for.}

\item{limits}{Numeric vector of length two, specifying the limits of density estimation allowed.}

\item{bw}{Bandwidth for density (default is nrd0).}

\item{cut}{A multiple of estimated bandwidth past the range of the data to use (default 3).}

\item{trim}{Should the density limits be trimmed per \code{limits}?}
}
\value{
A numeric vector of length 2 representing the min and max limits for density estimation.
}
\description{
Drawing a nice density plot that goes to zero can be challenging. This function guesses a
set of limits for that purpose.
}
\details{
Creating a density plot is a great way to see data distributions. However, it can be confusing
to see a plot that abruptly ends at a particular point, rather than going to zero. of course,
it may be the case that with no data to estimate how a distribution goes to zero it can
also confusing.

This function is designed to come up with
}
