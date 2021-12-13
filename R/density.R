#' Calculate density estimate of vector
#'
#' @description Calculates the density estimate, using [density()], of the given set of values.
#'
#' @details This function calculate the density estimate of the distribution. It is rather
#' simplified, in that it expects a min/max range for calculating the estimates. The
#' [stats::density()] function uses a heuristic for this purpose. See
#' [compute_density_limits()] for a tweaked version of this heuristic. The purpose is
#' to separate the two components, since density estimation can occur on subset of the
#' input data. In these cases, the estimation of limits (and density evaluation points)
#' can then differ which would be a problem.
#'
#' limits. This is a two-value vector that defaults to be just
#' past range of the data (see [stats::density()] for details.
#' Setting it to different values can be done - consider
#' the following example. Assume the data (in `x`) ranges from 0.5-1 but
#' the possible range of the variable can be 0-1. In this case, we may
#' want a density over the wider (0-1) range. It will be zero (or close)
#' much beyond the scale 0.5-1, but will look better as it tends toward 0.
#'
#'
#' @note The return result of this function is not exactly that of [stats::density()]. It
#' returns a [tibble::tibble()]. The columns of the return result are x, y and n.
#'
#' @param x
#' @param limits
#' @param bw
#' @param ...
#'
#' @return
#'
#' @examples
estimate_density <- function(x, limits, bw = "nrd0", ...) {

  f.bw <- switch(bw, nrd0 = bw.nrd0)

  d <- stats::density(x, bw = bw, from=limits[1], to=limits[2])
  tibble::tibble(x = d$x, y = d$y, n = 1:length(d$x))

}

#' Scale density estimates to 0/1 range
#'
#' @param y Numeric vector of values to scale
#'
#' @return A numeric vector scaled to 0/1 by the range of `y`.
#' @export
#'
#' @examples
scale_density <- function(y) {

  # This is the basic scaling, normalizing to range of 0..1
  (y - min(y)) / (max(y) - min(y))

}


#' Calculate density estimates across groups
#'
#' @description Estimate densities of `x` by each level of `by`.
#'
#' @details
#' Given a data.frame (`.data`) with values in `x`, estimate the density of `x`
#' separately for each of the levels of `by`.
#'
#' For instance, if `by` is `color` with values `c("blue","red","green")` then
#' compute densities for `x` separately for blue, red and green.
#'
#' The return result of this function is a data frame with three columns: x, y and
#' by. The x,y values are the results of estimating density (see [estimate_density()])
#' and the value of `by` is the specific level value from the `by` variable.
#'
#' @param .data A data frame to use for `x` and `by`.
#' @param x A variable to estimate density from.
#' @param by A grouping-type variable (e.g., factor) to calculate `x` densities by.
#' @param ... Any further parameters to [stats::density()].
#'
#' @return A data.frame with three columns: x, y and by.
#' @export
#'
#' @examples
estimate_density_groups <- function(.data, x, by, limits,  ...) {

  dg <- .data %>%
    # Group by the by variable first.
    dplyr::group_by({{ by }}) %>%
    # Then compute density estimates (by group).
    dplyr::summarize(.summary =
                       dplyr::bind_cols(
                         estimate_density({{ x }}, limits = limits, ...),
                         by = unique({{ by }})
                       ),
                     .groups = "drop_last"
    ) %>%
    # Extract summary only (x,y,by) and combine across by levels.
    dplyr::pull(.summary) %>%
    dplyr::bind_rows()

  if ( ! is_density_grouping_equal(dg))
    warn("Warning: Density estimates were not performed on consistent X coordinates.")

  dg
}

#' Title
#'
#' @param .data
#' @param x
#' @param by
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
estimate_max_density_by <- function(.data, x, by, limits, ...) {

  # Calculate the density by the by variable.
  d <- estimate_density_groups(.data, {{ x }},{{ by }}, limits, ...)

  stopifnot(is_density_grouping_equal(d))

  d %>%
    # Then for each density observation, select the entry with the largest
    # density estimate (y).
    dplyr::group_by(n) %>%
    dplyr::slice_max(y) %>%
    dplyr::ungroup()
}

#' Determine if density grouping(s) are equal
#'
#' @description A density grouping is a series of density estimates based on
#' another variable. This function tests if the density estimate x coordinates
#' are equal.
#'
#' @details A density estimation technique evaluates the density of a given
#' distribution at a grid of points. In the case of a density grouping, that
#' means each density estimation should have been done at the same grid of
#' points to be directly comparable. While that is not always required, this
#' function tests whether or not the density groupings are equal and thus
#' able to be compared.
#'
#' @note A density grouping is created by [estimate_density_groups()].
#'
#' @param d A data.frame from [estimate_density_groups()].
#'
#' @return
#' @export
#'
#' @examples
is_density_grouping_equal <- function(d) {
  # A very elaborate check to make sure that x's are the same, before
  # going and combining them.
  d %>%
    dplyr::group_by(n) %>%
    dplyr::summarize(identical = diff(range(x))==0) %>%
    dplyr::pull(identical) %>%
    all()
}

#' Compute density limits
#'
#' @description
#' Drawing a nice density plot that goes to zero can be challenging. This function guesses a
#' set of limits for that purpose.
#'
#' @details
#' Creating a density plot is a great way to see data distributions. However, it can be confusing
#' to see a plot that abruptly ends at a particular point, rather than going to zero. of course,
#' it may be the case that with no data to estimate how a distribution goes to zero it can
#' also confusing.
#'
#' This function is designed to come up with
#'
#' @param x Numeric vector of points to estimate density for.
#' @param limits Numeric vector of length two, specifying the limits of density estimation allowed.
#' @param bw Bandwidth for density (default is nrd0).
#' @param cut A multiple of estimated bandwidth past the range of the data to use (default 3).
#' @param trim Should the density limits be trimmed per `limits`?
#'
#' @return A numeric vector of length 2 representing the min and max limits for density estimation.
#' @export
#'
#' @examples
compute_density_limits <- function(x, limits=NULL, bw = "nrd0", cut = 3, trim = TRUE) {

  f.bw <- switch(bw, nrd0 = bw.nrd0)

  # density buffer is past last elements of the data.
  density_buffer <- f.bw(x) * cut

  # This implements somewhat confusing logic. The default range for density
  # is min +/- 3*bw() - see the density help page. However, we can include
  # limits which can be used instead. But, if the "trim" option is set,
  # then we take the smallest of the default range and the given ranges.
  value_min <-  min(x) - density_buffer
  value_max <-  max(x) + density_buffer
  if ( !is.null(limits)) {
    if ( trim ) {
      value_min <-  max(value_min, limits[1])
      value_max <-  min(value_max, limits[2])
     }# else {
    #   # Or use value_limits explicitly.
    #   value_min <- value_limits[1]
    #   value_max <- value_limits[2]
    # }
  }
  # NB: I do not do this anymore, but I am unsure of the impact on
  # figures, therefore I am leaving it in place for the time being.
  # The below calculates the density for the overall distribution,
  # We extend the range a bit to make it look nicer (hopefully going to 0)
  # The max/min ensures it stops at the builtin range (value_limit).
  # Note: the default kernel is bw.nrd0, the default value of cut is 3
  # based on existing R density defaults.
  #
  #     if ( trim ) {
  #     value_min<-max(value_min-bw(x)*cut, value_limits[1])
  #     value_max<-min(value_max+bw(x)*cut, value_limits[2])
  #   }

  c(value_min, value_max)

}
