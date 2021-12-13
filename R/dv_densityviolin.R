#' Title
#'
#' @note If using this function, one has to pay attention to the viewport
#' that it is drawn in. Namely this function draws using the scale of the
#' x values provided, even if the viewport doesn't support the range.
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
dv_densityviolin <- function(.data,
                            group,
                            x,
                             value_limits=NULL,
                              name = "", # should be the category.
                              vp = "dviolin_plot::violin_panel",
                             violin_end_width = 1,
                             violin_width = 1,
                             vex = 0.8,
                             scale = c("width","area"),
                             # Color scale somehow
                             density.color.high="yellow",
                             density.color.mid="darkgreen",
                             density.color.low="blue",
                             trim = FALSE,
                            color_by = NULL,
                            cut = 3,
                            bw = "nrd0",
                            color_map = NULL
                             ) {

  # y is a tibble that should have a single factor,.
  stopifnot(ncol(group)==1)
  group <- unlist(group)
  # use the numeric version as well.
  yn <- as.numeric(group)

  .x <- dplyr::pull(.data, {{ x }})

  scale <- match.arg(scale)
  # x should (perhaps) have values and labels? Or separate?
  # THIS HAS NOT BEEN ADDRESSED.
  # category.list<-levels(X$category)
  # values<-X$values[which(X$category==category.list[i])]
  #
  #
  #
  # if (color_by == "label") {
  #   local.labels<-factor(as.character(X$labels[which(X$category==category.list[i])]),levels=label_levels)
  # }
  #
  #
  # if (na.rm == TRUE) {
  #   values <- values[!is.na(values)]
  #   if (color_by=="label") {
  #     local.labels <- local.labels[!is.na(values)]
  #   }
  # }

  value_limits <- compute_density_limits(.x,
                                         limits = value_limits,
                                         bw = bw,
                                         cut = cut,
                                         trim = trim
  )

  # Calculate density of points
  d <- estimate_density(.x, value_limits, trim = trim)

  # Optionally scale the density
  if ( scale == "width" )
    d$y <- scale_density(d$y)

  if ( !rlang::quo_is_null(rlang::enquo(color_by))) {
    # This is an involved bit of dplyr magic. The idea is to calculate
    # densities by each color_by level, then pick the level corresponding
    # to the largest density value at each evaluated point.
      #
      #fill <- calculate_color_by()
    maxd <- estimate_max_density_by(.data, x = {{ x }}, by = {{color_by}},
                                    limits = value_limits)

    if ( is.null(color_map))
      color_map<-magrittr::set_names(c("red","blue","green"),
                                     dplyr::pull(dplyr::distinct(maxd, by), by))

    fill <- maxd %>%
      dplyr::left_join(tibble::enframe(color_map, name = "by", value="color"), by="by") %>%
      dplyr::pull("color")

    } else {
      color_fun <- circlize::colorRamp2(
        breaks = c(quantile(d$y, 0.01), quantile(d$y,0.5), quantile(d$y, 0.99)),
        colors = c(density.color.low, density.color.mid, density.color.high)
      )
      fill = color_fun(d$y)
    }
    # circlize::colorRamp2(seq(quantile(X$values,0.01),
    #                           quantile(X$values,0.99), length.out=3),
    #                       colors=c(density.color.low, density.color.mid,
    #                                density.color.high))


  # Draw the polygons defined above. The data is a
  # bunch of small polygons with individual colors. Each polygon has four points
  # representing the four edges (we are actually doing a rectangle).
  grid::gTree(name = sprintf("group_%s_violin",group), vp = NULL,
              children =
                grid::gList(
                  grid::rectGrob(x = d$x,
                                 y = yn,
                                 width = d$x[2] - d$x[1],
                                 height = d$y * vex,
                                    # fill polygons with plot color (a vector).
                                    # col=0 turns off borders
                                    gp=grid::gpar(fill=fill, col=fill),
                                 vp = vp,
                                    default.units="native",

                )
  )
  )

}


