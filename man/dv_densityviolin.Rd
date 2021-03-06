% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dv_densityviolin.R
\name{dv_densityviolin}
\alias{dv_densityviolin}
\title{Title}
\usage{
dv_densityviolin(
  .data,
  group,
  x,
  value_limits = NULL,
  name = "",
  vp = "dviolin_plot::violin_panel",
  violin_end_width = 1,
  violin_width = 1,
  vex = 0.8,
  scale = c("width", "area"),
  density.color.high = "yellow",
  density.color.mid = "darkgreen",
  density.color.low = "blue",
  trim = FALSE,
  color_by = NULL,
  cut = 3,
  bw = "nrd0",
  color_map = NULL
)
}
\arguments{
\item{y}{}
}
\value{

}
\description{
Title
}
\note{
If using this function, one has to pay attention to the viewport
that it is drawn in. Namely this function draws using the scale of the
x values provided, even if the viewport doesn't support the range.
}
