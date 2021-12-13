#' Purrr style map across categories with grobs.
#'
#' @description A function similar to [dplyr::group_map()], but returning
#' a grob list ([grid::gList()]).
#'
#' @details
#' The map-style functions are great for applying a function across
#' a series of data, particularly the [dplyr::group_map()].
#'
#' This function wraps [dplyr::group_map()], so that a [grid::gList()] is returned
#' for use in grob-related functions. So the class of the list is set to gList,
#' as is done in the [grid::gList()] function.
#'
#' A mapped function is expected to take two fixed parameters, the grouped data frame
#' and a tibble representing the group_by variable names. The function must also take
#' a `x` parameter that specifies the x variable (assuming the y is the group_by variable).
#'
#' @note The `by` must be a factor, since we extract the
#' numeric value currently to use in the called function. The assumption
#' would be that `by` will represent a coordinate within the grob (though
#' it's not required to).
#'
#' @param .data A data frame to map. Must include columns referred to in `by` and `values`.
#' @param .f A function to apply to groupings of .data (grouped by `by`).
#' @param by The name of the grouping variable.
#' @param values The name of the value variable to pass to `f`
#' @param ... Any additional parameters to pass to `f`.
#'
#' @return A [grid::gList()] of [grid::grob()] that result from calling `f` from `X$values`, grouped
#' by `X$by`.
#'
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
map_grob<- function(.data, .f, by, values,  ...) {

  .data %>%
    dplyr::group_by({{ by }}) %>%
    # Not sure why, but group_map doesn't allow {{ }} in the args of the call, so we pass
    # a quosure which can be evaluated later.
    dplyr::group_map(.f = .f, x = rlang::enquo(values), ...) %>%
    magrittr::set_class("gList")
}


