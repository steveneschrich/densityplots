#' Create boxplot to overlay on densityviolin
#'
#' @description Create a boxplot from a vector of data to overlay on the densityviolin plot.
#'
#' @param .data Data from to generate boxplot from
#' @param x The variable to use as boxplot values.
#' @param y Factor variable representing the y coordinate to plot box.
#' @param box_width
#' @param vp
#'
#' @return
#' @export
#'
#' @examples
dv_boxplot <- function(.data,
                       group,
                       x,
                       box_width = 1,
                       vp = "dviolin_plot::violin_panel",
                       box_col = "black",
                       box_fill = "black",
                       median_col = "red",
                       show_outliers = FALSE,
                       coef = 1.5,
                       pch = 16) {

  stopifnot(ncol(group)==1)
  # y is a tibble that should have a single factor,.
  group <- unlist(group)
  # use the numeric version as well.
  yn <- as.numeric(group)

  # Calculate boxplot stats
  z <- grDevices::boxplot.stats(dplyr::pull(.data, {{ x }} ), coef = coef)

  grid::gTree(name = sprintf("group_%s_boxplot",group), vp = NULL,
              children =
                grid::gList(
                  grid::linesGrob(
                    name = "boxplot_lowerwhisker",
                    vp = vp,
                    y = grid::unit(c(yn, yn),"native"),
                    # lower whisker, lower hinge.
                    x = grid::unit(z$stats[1:2], "native"),
                    gp=grid::gpar(lineend="butt", lwd = 1)
                  ),
                  grid::linesGrob(
                    name = "boxplot_upperwhisker",
                    vp = vp,
                    y = c(yn, yn),
                    x = z$stats[4:5], # upper hinge, upper whisker.
                    gp=grid::gpar(lineend="butt", lwd = 1),
                    default.units="native"
                  ),
                  grid::polygonGrob(
                    name = "boxplot_box",
                    vp = vp,
                    y = c(
                      yn - box_width,
                      yn + box_width,
                      yn + box_width,
                      yn - box_width
                    ),
                    x = c(
                      z$stats[2], z$stats[2], # upper hinge
                      z$stats[4], z$stats[4]  # lower hinge
                    ),
                    gp=grid::gpar(col = box_col, fill = box_fill),
                    default.units="native"),
                  grid::linesGrob(
                    name = "boxplot_medianbar",
                    vp = vp,
                    y = c(
                      yn - box_width,
                      yn + box_width
                    ),
                    x = c(z$stats[3], z$stats[3]), # median
                    gp=grid::gpar(
                      lineend="butt",
                      lwd = 4,
                      col=median_col
                    ),
                    default.units="native"
                  ),

                  if (show_outliers && length(z$out)>0)
                    grid::pointsGrob(
                      name = "boxplot_outliers",
                      vp = vp,
                      y = rep(yn, length(z$out)),
                      x = z$out,
                      pch = pch,
                      size = grid::unit(0.25,"char"),
                      default.units="native")
                  else
                    NULL
                )
  )

}

