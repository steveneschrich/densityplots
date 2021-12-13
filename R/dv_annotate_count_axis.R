#' Annotate counts axis
#'
#' @param counts The counts to annotate plot with
#' @param gp Graphical parameters for labels (see [grid::gpar()]).
#' @param prefix A prefix for label counts (e.g., N)
#' @param sep A separator between prefix and count (e.g., =)
#' @param title The title to use (default is N)
#' @param vp Viewport to plot in (see [grid::viewport()])
#'
#' @return A grob representing an annotated axis.
#' @export
#'
#' @examples
#' \dontrun{
#' dv_annotate_count_axis(counts=c(2,5,1,2), prefix="N", sep=":", title="Counts", vp = "panel")
#' }
dv_annotate_count_axis <- function(counts, gp = grid::gpar(), prefix = "N", sep = "=",title = "N",
                      vp = NULL) {
  labels<-paste0(prefix, sep, counts)
  n <- length(counts)

  grid::gTree(name = "count_annotation", vp = NULL ,
              children = grid::gList(
                grid::textGrob(
                  name = "n_labels",
                  label = labels,
                  y = 1:n,
                  default.units = "native",
                  x = grid::unit(0.5, "npc"),
                  just = "right",
                  gp = gp, vp = vp),
                grid::textGrob(
                  name = "n_title",
                  label = title,
                  y = n+1,
                  default.units = "native",
                  x=grid::unit(0.5, "npc"),
                  just = "right",
                  gp = grid::gpar(fontface = "bold"),
                  vp=vp
                )
              )
  )
}
