#
#
#' Create density-based violin plot
#'
#' @details
#' Based on the code from plotrix v3.6-5
#'
#' # Input matrix X should be a data frame with
#'  X$category
#'  X$values
#'  X$labels (optional, if color_by is "label")
#' The defaults are changed:
#' bw=0.35 changed to "SJ", a recommended density bandwidth selector
#' other options: bw= c("nrd0","nrd","ucv","bcv","SJ")
#'
#'
#' @param X
#' @param na.rm
#' @param bw
#' @param violin_width
#' @param violin_end_width
#' @param equal_width
#' @param box_width
#' @param box_col
#' @param show_outliers
#' @param pch
#' @param range
#' @param value_limit
#' @param value_label
#' @param title
#' @param show_title
#' @param main
#' @param show_main
#' @param title_gp
#' @param category_limit
#' @param category_label
#' @param category_names
#' @param show_n
#' @param n_prefix
#' @param n_gp
#' @param median_col
#' @param plot_mean
#' @param mean_pch
#' @param mean_pch_col
#' @param summary.function
#' @param max.colors
#' @param color_by
#' @param density.color.high
#' @param density.color.mid
#' @param density.color.low
#' @param label_colors
#' @param label.color.high
#' @param label.color.low
#' @param newpage
#' @param grand_median_line
#' @param grand_median_gp
#' @param cex
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
dviolin_plot<-function (X = data.frame(x=rnorm(50, mean=10, sd=1), y=c(1)),
                        values = x,
                        category = y,

                        na.rm = TRUE, bw = "SJ",
                        violin_width = 0.8, violin_end_width = 0.005, equal_width = TRUE,
                        box_width = 0.03, box_col = "black", show_outliers = FALSE,
                        pch = 1, boxplot_range = 1.5,
trim = TRUE,
                        value_limits,  value_label = "",

                        # Title options
                        title = main,
                        show_title = show_main,
                        main = "Violin Plot",
                        show_main=TRUE,
                        title_gp = grid::gpar(fontface="bold"),


                        # Categories: How the y axis is split up.
category_gp = grid::gpar(cex=cex), # Category label parameters
                        category_limits,
                        category_label = "",
                        category_names,
                        show_n=TRUE, # Show the N= tag
                        n_prefix="", # What is the prefix
                        n_sep="", # What is the separator.
                        n_gp = grid::gpar(cex=cex),


                        median_col = "white",
                        show_means = FALSE, mean_pch = 19, mean_pch_col = "yellow",
                        summary_function=median,
                        max.colors=50,
                        # color_by:
                        #   density is just coloring the density of the distribution
                        #   label is coloring by the label in X$labels
                       #color_by=c("density","label"),
color_by=NULL,
                        density.color.high="yellow",
                        density.color.mid="darkgreen",
                        density.color.low="blue",
                        label_colors=NULL,
                        label.color.high="red",
                        label.color.low="blue",

                        newpage=FALSE,

                        order_summary = median,
                        # On the violin plot, a grand median (of all data) can be plotted.
                        grand_summary = NULL,
                        grand_summary_gp = grid::gpar(lty=2),

                        cex=0.75,

                        show_boxplots = TRUE,
                        show_violins = TRUE,
                        draw = FALSE,
desc = TRUE,
                        ...)
{


  # This also helps by immediately
  # throwing an error if the category and values columns do not appear in X.
  dplyr::select(X, {{category}}, {{values}})

  # Order categories by the summary of values.
  # NOTE: The parameter desc *must* be reversed so the plot shows in the expected
  # order.
  X <- dplyr::mutate(X, {{ category }} := forcats::fct_reorder(.f = {{ category }},
                                                               .x = {{ values }},
                                                            .fun = order_summary,
                                                            .desc = !desc)
  )

  # Category information
  cati <- X %>%
    dplyr::arrange(as.numeric({{category}})) %>%
    # Counts (in n) per category.
    dplyr::count({{ category }}) %>%
    # at is the position of the violin in normal graph order (1,2,etc)
    dplyr::mutate(at = dplyr::row_number())

  # Fill in missing values with intelligent defaults.
  if (missing(category_limits))
    category_limits <- c(1 - violin_width/2, nrow(cati) + violin_width/2)
  if (missing(value_limits))
    value_limits <- range(dplyr::pull(X, {{values}}), na.rm = TRUE)

  if (missing(category_names)) {
    category_names <- dplyr::pull(cati, {{ category }})
  }
  if ( !is.null({{color_by}}) && ! is.factor(dplyr::pull(X, {{color_by}}))) {
    X <- X %>%
    dplyr::mutate({{color_by}} := factor({{color_by}}))
  }

  # Draw the graph layout/axes/etc
  if (newpage) grid::grid.newpage()

  # Setup the plot area (see dv_define_plot for names).
  main_plot <- dv_define_plot(
    xlim = value_limits,
    ylim = c(1,nrow(cati))
  )

  # Create the grobs.
  grobs <- grid::gTree(name = "main_grob",
                       childrenvp = main_plot,
                       children = NULL
  )

  # Optionally draw title.
  if ( show_title )
    grobs <- grid::addGrob(grobs, child =
                             dv_title(
                               label=title,
                               gp = title_gp,
                               vp = "dviolin_plot::title_panel"
                             )
    )


  # Draw the value legend triangle
  grobs <- grid::addGrob(grobs, child =
                           dv_value_legend(label = value_label,
                                           order = "increasing",
                                           vp = "dviolin_plot::gutter_panel"
                           )
  )

  # Add in N= counts
  if (show_n) {
    grobs <- grid::addGrob(grobs, child =
                             dv_annotate_count_axis(cati$n,
                                       gp = n_gp,
                                       prefix = n_prefix,
                                       sep = n_sep,
                                       vp = "dviolin_plot::n_panel"
                             )
    )
  }

  grobs <- grid::addGrob(grobs, child =
                         dv_plot_area(
                           gp = category_gp,
                           vp = "dviolin_plot::violin_panel",
                           ylabels = category_names
                         )
  )

  # Draw the violins
  if ( show_violins ) {
    grobs <- grid::addGrob(grobs, child =
                             grid::gTree(
                               children =
                                 map_grob(X,
                                          .f = dv_densityviolin,
                                          by = {{ category }},
                                          color_by = rlang::enquo(color_by),
                                        values = {{ values }},
                                        vp = "dviolin_plot::violin_panel",
                                        value_limits = value_limits,
                                        trim = trim,
                                        ...


                                        )
                             ))
  }


  # Draw the boxplots (if requested)
  if ( show_boxplots )
    grobs <- grid::addGrob(grobs, child =
                             grid::gTree(
                               children =
                                 map_grob(X,
                                          .f = dv_boxplot,
                                          by = {{ category }},
                                          values = {{ values }},
                                        vp = "dviolin_plot::violin_panel",
                                        box_col = box_col,
                                        median_col = median_col,
                                        box_width = box_width * violin_width,
                                        show_outliers = show_outliers,
                                        coef = boxplot_range)
                             ))


  if ( show_means ) {
    grobs <- grid::addGrob(
      grobs,
      child =
        grid::gTree(
          children =
            map_grob(X,
                     .f = dv_plotmeans,
                     by = {{ category }},
                     values = {{ values }},
                   vp = "dviolin_plot::violin_panel",
                   gp = grid::gpar(col = mean_pch_col )
            )
        )
    )
  }


  # Add a grand median line across all violins.
  if ( !is.null(grand_summary) )
    grobs <- grid::addGrob(
      grobs,
      child = dv_grand_summary(
        x = dplyr::pull(X, {{ values }}),
        gp = grand_summary_gp,
        vp = "dviolin_plot::violin_panel",
        summary_function = grand_summary
      )
    )

  if ( draw )
    grid::grid.draw(grobs)

  grobs
}








#' Define density violin plot area
#'
#'
#' @param xlim
#' @param ylim
#'
#' @return A [grid::vpTree] defining the density violin plot.
#'
#' @examples
dv_define_plot <- function(xlim, ylim) {

  # Define the whole plot tree (3x3 grid).
  grid::vpTree(
    parent = grid::viewport(
      # Plot area
      name = "dviolin_plot",
      # Define layout of plotting area.
      layout = grid::grid.layout(
        nrow = 3,
        ncol = 3,
        # Proportional widths: 20%, 50%, 10%.
        widths=grid::unit(c(20, 50, 10), c("null", "null", "null")),
        # Heights: 1 line, remaining area, 1 line.
        heights=grid::unit(c(1, 1, 4), c("lines", "null", "lines"))
      )
    ),
    # Each child part of the grid is defined below, so they
    # can be selected later in code.
    children = grid::vpList(
      grid::viewport(
        layout.pos.col = 2, layout.pos.row = 1,
        name ="title_panel"
      ),
      grid::viewport(
        layout.pos.col = 2, layout.pos.row = 2,
        name = "violin_panel",
        # Extend the x axis range, to provide some buffer on the edges.
        xscale = grDevices::extendrange(xlim),
        # Expand the y axis to pad bottom (a little) and top (more).
        yscale = c(-0.5,max(ylim)+1.5),
        # Turn off clipping, not sure why yet.
        clip="inherit"
      ),
      grid::viewport(
        layout.pos.col = 3, layout.pos.row = 2,
        name = "n_panel",
        # Expand the y axis to pad bottom (a little) and top (more).
        yscale = c(-0.5,max(ylim)+1.5),
      ),
      grid::viewport(
        layout.pos.col = 2, layout.pos.row = 3,
        name = "gutter_panel",
        # For this panel, since it mirrors the violin_panel, it needs the same
        # expansion factor given to the violin_panel.
        xscale=grDevices::extendrange(xlim)
      )
    )
  )
}


#' Draw value legend of density violin plot
#'
#' @description Creates a [grid::grob()] representing a triangular graphic
#' representing the increasing (or decreasing) intensity across the axis.
#'
#' @param g An existing grob/gTree
#' @param label
#' @param order
#' @param fill
#' @param vp
#'
#' @return A [grid::grob()] representing the graphic.
#'
#' @examples
dv_value_legend <- function(label = "",
                            order = c("increasing","decreasing"),
                            fill = "black",
                            vp = "dviolin_plot::gutter_panel") {
    heights <- c(0.5, 0.3)
    if ( order != "increasing" ) heights = rev(heights)

    grid::gTree(name = "value_legend", vp = NULL ,
                children = grid::gList(
                  grid::polygonGrob(name = "value_graphic",
                                    x = c(0, 1, 1, 0),
                                    y = c(0.3, 0.3, heights),
                                    gp = grid::gpar(fill = fill),
                                    vp = vp
                  ),
                  grid::textGrob(name = "value_name",
                                 x = 0.5,
                                 y = 0.2,
                                 label = label,
                                 vp = vp
                  )
                )
    )
}



#' Title
#'
#' @param label
#' @param gp
#' @param vp
#'
#' @return
#' @export
#'
#' @examples
dv_title <- function(label = "", gp = grid::gpar(), vp = "dviolin_plot::title_panel") {
  grid::gTree(name = "title", vp = NULL,
              children = grid::gList(
                grid::textGrob(
                  name = "title_text",
                  label = label,
                  gp = gp,
                  vp = vp
                )
              )
  )
}


#' Title
#'
#' @details
#' TODO:   # Add in N underline at top (bold): col=3,row=1
#' @param ylabels
#' @param gp
#' @param vp
#'
#' @return
#'
#' @examples
dv_plot_area <- function(ylabels = c(),
                         gp = grid::gpar(),
                         vp = "dviolin_plot::violin_panel") {

 grid::gTree(name = "plot_area", vp = NULL,
             children = grid::gList(
                grid::rectGrob(name = "plot_border",
                               vp = vp,
                               gp = grid::gpar(width=4)),
                # Create x axis (no labels)
                grid::xaxisGrob(name = "x_axis",
                                vp = vp,
                                edits =
                                  grid::gEdit(gp = grid::gpar(lty=0))
                ),
                # Create left y axis
                grid::yaxisGrob(name = "y_axis",
                                at = 1:length(ylabels),
                                edits =
                                  grid::gEdit(gp = grid::gpar(lty=0)),
                                label = ylabels,
                                gp = gp,
                                vp = vp)
             )
 )
}



dv_plotmeans <- function(x, y, name = "", vp = NULL, mean_pch=16, gp=grid::gpar()) {

  grid::gTree(
    name = sprintf("boxplot_%d_mean",y),
    vp = NULL,
    children = grid::gList(

      grid::pointsGrob(
        name = "boxplot_mean",
        vp = vp,
        y = y,
        x = mean(x),
        pch = mean_pch,
        gp=gp,
        default.units="native")
    )
  )
}







