#' Create a Slope Class
#'
#' The slope class constructor appends the Slope class to the slope data frame.
#' The constructor validates the slope data frame, and the slope class provides
#' functionality to plot the elevation and slope plot, and to linearize the
#' slope data
#' @name Slope
#' @param dataframe A slope dataframe
#' @export
#' @rdname Slope
#' @examples
#' fpath_slp <- system.file("extdata", "071000090603_2.slp", package="WEPPR")
#' slp <- read_slp(fpath_slp)
new_Slope <- function(x = data.frame()) {
  stopifnot(is.data.frame(x))

  # validate slope class
  # find rows where difference in n is 1 and difference in slope is zero
  diff_sum <- x %>%
    group_by(n) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(diff = slope - lag(slope), diff_n = n - lag(n)) %>%
    filter(diff_n == 1) %>%
    summarize(valid = sum(diff)) %>%
    pull()

  # difference = zero means the all slope between OFEs match
  if (diff_sum != 0) {
    stop("Slope does not match across OFE", call. = FALSE)
  }

  structure(x, class = c("data.frame", "Slope"))
}

#' Modify Slope object by adding coefficients for slope and elevation calculations
#'
#' The Slope object contains slope information at specific distances relative
#' to the start of each overland flow element (OFE) that comprises the hillslope.
#' This function will augment the columns in the Slope object to include the
#' coefficients for the slope and elevation functions.
#'
#' To calculate the slopes and elevation at different distances, it is
#' convenient to calculate the total distance for the hillslope which is found
#' by adding the total length of all previous OFEs to the distance for the
#' current OFE. This function adds a column called \code{x} that
#' represents this total distance.
#'
#' The slope at intermediate total distance not explicitly specified by the
#' slope object are found by linear interpolation between the slope endpoints.
#' Specifically, let s_i be the slope at a distance x_i, then the slope at a
#' point x, s(x), where x_i < x < x_{i+1} is
#'
#'     s(x) = m_i (x-x_i) + b_i
#'
#' where
#'
#'     b_i = s_i and m_i = (s_{i+1}-s_i) / (x_{i+1}).
#'
#' This function calculates these \code{b} and \code{m}.
#'
#' The elevation at an intermediate total distance is the integral of the
#' slope function. We define the start of the elevation as (0,0). Then the
#' elevation at a point x, e(x), where x_i < x < x_{i+1} is
#'
#'    e(x) = m_i (x-x_i)^2 + b_i (x-x_i) + c_i
#'
#' where
#'
#'    c_i = y_i.
#'
#' @param slp A Slope object
#' @return A \code{data.frame} that is a modified version of the Slope object
#' with additional columns x, m, b, and c.
#' @export
#' @seealso \code{\link{plot.Slope}}, \code{\link{expand_slp}}, \code{\link{remove_slp_transitions}}
#' @examples
#' slp <- read_slp(system.file("extdata", "071000090603_2.slp", package="WEPPR"))
#' integrate_slp(slp)
#'
integrate_slp <- function(slp) {
  slp <- remove_slp_transitions(slp)

  n <- nrow(slp)

  slp$x <- calculate_total_distance(slp)
  d <- diff(slp$x)

  slp$b <- slp$slope
  slp$m <- c(diff(slp$slope) / d, NA)

  slp$c <- cumsum(c(0, slp$m[-n] * d ^ 2 / 2 + slp$b[-n] * d))

  return(slp)
}

#' Expand Slope object to calculate slope and elevation at specific distances
#'
#' This function will calculate the slope and elevation of a Slope object for
#' specific distances
#'
#' @param slp A Slope object
#' @param distances a numeric vector of distances
#' @param n a positive integer indicating how many equally-spaced distances to
#'     calculate. Ignored if \code{distances} is provided.
#' @return a data.frame containing distance, slope, and elevation
#' @seealso \code{\link{plot.Slope}}, \code{\link{integrate_slp}}, \code{\link{remove_slp_transitions}}
#' @examples
#' slp <- read_slp(system.file("extdata", "071000090603_2.slp", package="WEPPR"))
#' expand_slp(slp)
#'
expand_slp <- function(slp, distances = NULL, n = 1001) {
  slp <- integrate_slp(slp)

  if (is.null(distances))
    distances <- seq(0, max(slp$x), length = n)

  elevation <- Vectorize(function(x, slope) {
    i <- sum(x >= slope$x)
    xd <- x - slope$x[i]
    return(as.numeric(slope$m[i] * xd ^ 2 / 2 + slope$b[i] * xd + slope$c[i]))
  }, vectorize.args = "x")

  slope <- Vectorize(function(x, slope) {
    i <- sum(x >= slope$x)
    return(as.numeric(slope$m[i] * (x - slope$x[i]) + slope$b[i]))
  }, vectorize.args = "x")

  return(data.frame(
    x = distances,
    elevation = -elevation(distances, slp),
    slope = slope(distances, slp)
  ))
}

#' Remove first row for each new OFE in a Slope object or Soil object
#'
#' In calculating slope and elevation, it is convenient if the Slope object
#' does not have the first row for each new OFE. This function will remove that
#' first row.
#'
#' @param slp A Slope object
#' @return A modified Slope object with the first row of each new OFE removed.
#' @seealso \code{\link{plot.Slope}}, \code{\link{expand_slp}}, \code{\link{integrate_slp}}
#' @examples
#' slp <- read_slp(system.file("extdata", "071000090603_2.slp", package="WEPPR"))
#' remove_slp_transitions(slp)
#'
#' remove_slp_transitions(data.frame(n = rep(1:3, times = c(3,4,5)))
remove_slp_transitions <- function(slp) {
  slp[!diff(c(1, slp$n)), ]
}

#' Calculate the total distance
#'
#' By default, Slope objects include only the distance for each OFE rather
#' than the total distance in the hillslope. This function calculates the
#' total distance.
#'
#' @param slp A slope object
#' @return A numeric vector of total distances
#' @seealso \code{\link{calculate_total_distance}}, \code{\link{integrate_slp}}, \code{\link{expand_slp}}, \code{\link{remove_slp_transitions}}
#' @export
#' @examples
#' slp <- read_slp(system.file("extdata", "071000090603_2.slp", package="WEPPR"))
#' calculate_total_distance(slp)
#'
calculate_total_distance <- function(slp) {
  total_distance = numeric(length = nrow(slp))
  ofe <- 1
  offset <- 0
  for (i in seq_len(nrow(slp))) {
    if (ofe != slp$n[i]) {
      ofe <- ofe + 1
      offset <- total_distance[i - 1]
    }
    total_distance[i] <- slp$distance[i] + offset
  }
  total_distance
}

#' Plots the slope and elevation from a Slope object
#'
#' The Slope object contains distances and slopes. The remainder of the slopes
#' need to be linearly interpolated. These linearly interpreted slopes are the
#' derivative of the elevation (hillslope) profile. We start each hillslope at
#' the point (0,0).
#'
#' @param slp a Slope object
#' @param n (optional) a positive integer indicating how many points to evaluate for drawing
#' @param plots (optional) a charcter vector indicating whether to draw the "slope", "elevation", or both
#' @return a gtable containing plots of slope or elevation
#' @export
#' @examples
#' slp <- read_slp(system.file("extdata", "071000090603_2.slp", package="WEPPR"))
#' plot(slp, plots=c("slope"))
#' plot(slp, plots=c("elevation"))
#' plot(slp)
#'
plot.Slope <- function(slp,n = 1001, plots = c("slope", "elevation")) {

  if (!require(ggplot2))
    stop("You must install the 'ggplot2' package.")

  if (!require(gridExtra))
    stop("You must install the 'gridExtra' package.")

  slp_expanded <- expand_slp(slp)

  g_slope <- ggplot(slp_expanded, aes(x = x, y = slope)) +
    geom_line() +
    theme_minimal() +
    labs(x = "distance (m)", y = "slope", title = "Slope (linear interpolation)")

  g_elevation <- ggplot(slp_expanded, aes(x = x, y = elevation)) +
    geom_line() +
    theme_minimal() +
    labs(x = "distance (m)", y = "elevation", title = "Elevation")


  # both plots
  if ("slope" %in% plots & "elevation" %in% plots) {
    return(gridExtra::grid.arrange(g_slope, g_elevation))
  }

  # slope plot
  if ("slope" %in% plots) {
    return(g_slope)
  }

  # elevation plot
  if ("elevation" %in% plots) {
    return(g_elevation)
  }
}

#' Linearize the slope data file
#'
#' @param slp_class A slope object
#'
#' @examples
#' slp <- read_slp(system.file("extdata", "071000090603_2.slp", package="WEPPR"))
#' linearized_slp <- linearize_slp(slp)
linearize_slp <- function(slp_class) {
  remove_trailing0_pattern <- '^(\\.\\d*?[1-9])0+$'

  slp_df <- df %>%
    group_by(n) %>%
    mutate(slp_distance = max(distance)) %>%
    ungroup() %>%
    select(slope, slp_distance) %>%
    mutate(ID = paste(
      "slp_slope",
      sep = "_",
      str_remove(round(1:n() / n(), digits = 3), remove_trailing0_pattern)
    )) %>%
    pivot_wider(names_from = ID, values_from = slope) %>%
    summarise_all(sum, na.rm = T)

  slp_df
}
