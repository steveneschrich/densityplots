#' Add grand summary (e.g., grand median) to plot.
#'
#' @param x
#' @param gp
#' @param vp
#' @param summary_function
#' @return
#' @export
#'
#' @examples
dv_grand_summary <- function(x, gp=grid::gpar(), vp = NULL, summary_function = median, ...) {

  s <-summary_function(x, na.rm = TRUE, ...)

  grid::gTree(
    name = "grand_summary",
    vp = NULL,
    children = grid::gList(

      grid::linesGrob(
        name = "grand_summary_line",
        x = grid::unit( c(s, s), units = "native"),
        y = grid::unit( c(0, 1), units = "npc"),
        gp = gp,
        vp = vp
      )
    )
  )

}
