#' Create a Climate Class
#'
#' The climate class constructor appends the Climate class to the climate data frame.
#' The constructor validates the climate data frame, and the climate class provides
#' functionality to plot the instantaneous and cumulative precipitation,
#' and to interpolate the climate data
#' @name Climate
#' @param cli A list of data frames
#' @return Climate object with class \code{Climate} and \code{data.frame}
#' @rdname Climate
#' @examples
#' fpath_cli <- system.file("extdata", "092.63x040.90.cli", package="WEPPR")
#' (cli <- read_cli(fpath_cli))

new_Climate <- function(cli = list()) {
  stopifnot(is.list(cli))

  ## validate data frame with ppt values
  cli_ppt <- cli$breakpoints
  stopifnot(is.data.frame(cli_ppt))

  # validate there are no negative values
  if (any(cli_ppt < 0)) {
    stop("Negative values exist in slope data", call. = FALSE)
  }

  # validate time between 0 and 24
  if (any(cli_ppt$timem >= 24))
    stop("time not within 0 and 24 range")

  class(cli) <- append("Climate", class(cli))

  return(structure(cli))
}

#' Calculates the instantaneous precipitation (precipitation / time in hours)
#'
#' @param cli A Climate object
#' @return A \code{data.frame} that is a modified version of the Climate object
#' with additional columm m
#' @seealso \code{\link{plot.Climate}}, \code{\link{expand_cli}}
#' @examples
#' cli <- read_cli(system.file("extdata", "092.63x040.90.cli", package="WEPPR"))
#' integrate_cli(cli)
#'
integrate_cli <- function(cli, n = 24) {
  n <- nrow(cli)
  t <- diff(cli$timem) * (n / 24) # to handle different intervals
  cli$m <- c(diff(cli$pptcum) / t, NA)

  return(cli)
}


#' Expands Climate object to calculate ppt and inst ppt. at specific hours
#'
#' @param cli A Slope object
#' @param n (optional) a positive integer indicating how many equally-spaced time to
#'     calculate.
#' @return a data.frame containing time, ppt, and inst. ppt
#' @seealso \code{\link{plot.Slope}}, \code{\link{integrate_cli}}
#' @examples
#' cli <- read_cli(system.file("extdata", "092.63x040.90.cli", package="WEPPR"))\
#' expand_cli(cli)
#'
expand_cli <- function(cli, n = 1001) {
  cli <- integrate_cli(cli)

  # hour 0 to 23:59:59
  time <- seq(0, 23.997, length = n)

  ppt_cum <- Vectorize(function(time, cli) {
    i <- sum(time >= cli$timem)

    if (i == 0) {
      return (0)
    }
    else {
      return(as.numeric(cli$m[i] * (time - cli$timem[i]) + cli$pptcum[i]))
    }

  }, vectorize.args = "time")

  ppt_inst <- Vectorize(function(time, cli) {
    i <- sum(time >= cli$timem)

    if (i == 0) {
      return (0)
    }
    else {
      return(as.numeric(cli$m[i]))
    }

  }, vectorize.args = "time")

  return(data.frame(
    time = time,
    date = rep(cli$date[1], n),
    ppt = unlist(ppt_cum(time, cli)),
    ppt_inst = unlist(ppt_inst(time, cli))
  ))
}


#' Plots the ppt and inst. ppt from a Climate object
#'
#' @param cli a Climate object
#' @param date a date string value with format "YYYY-MM-DD"
#' @param n (optional) a positive integer indicating how many points to evaluate for drawing
#' @param plots (optional) a charcter vector indicating whether to draw the "ppt_inst", "ppt_cum", or both
#' @return a gtable containing plots of slope or elevation
#' @seealso \code{\link{expand_cli}}
#' @export
#' @examples
#' cli <- read_cli(system.file("extdata", "092.63x040.90.cli", package="WEPPR"))
#' plot(cli)
#' plot(cli, date = "2017-09-21", plots=c("ppt_inst"))
#' plot(cli, date = "2017-09-21", plots=c("ppt_cum"))
#' plot(cli, date = "2017-09-21")
#'
plot.Climate <- function(cli, n = 1001, Date = NULL, plots = c("ppt_inst", "ppt_cum")) {
  if (!require(ggplot2))
    stop("You must install the 'ggplot2' package.")

  if (!require(gridExtra))
    stop("You must install the 'gridExtra' package.")

  cli <- as.data.frame(cli$breakpoints)

  if (is.null(Date)) {
    ppt_day <- cli %>%
      group_by(date) %>%
      summarize(max_ppt = max(pptcum)) %>%
      ggplot(aes(x = date, y = max_ppt)) +
      geom_line()

    return(ppt_day)
  }

  is_date = function(x) {
    formatted = try(as.Date(x, "%Y-%m-%d"), silent = TRUE)
    is_date = as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
    is_date[is.na(x)] = NA  # Insert NA for NA in x
    return(is_date)
  }

  # validate Date string is a date
  if (!is_date(Date)) {
    stop("Date given is not a Date or does not follow YYYY-MM-DD format", call. = FALSE)
  }

  # validate Date exist in data frame
  if (!any(Date == cli$date)) {
    stop("Date given not within cli data frame", call. = FALSE)
  }

  expanded_cli <- cli %>%
    filter(date == Date) %>%
    expand_cli(n = n) %>%
    replace_na(list(ppt_inst = 0)) %>%
    fill(ppt, .direction = 'down')

  ppt_inst <- ggplot(expanded_cli, aes(x = time, y = ppt_inst)) +
    geom_line() +
    theme_minimal() +
    labs(x = "time (hours)", y = "ppt_inst", title = "Instantaneous precipitation")

  ppt_cum <- ggplot(expanded_cli, aes(x = time, y = ppt)) +
    geom_line() +
    theme_minimal() +
    labs(x = "time (hours)", y = "ppt", title = "Cumulative Precipitation")


  # both plots
  if ("ppt_inst" %in% plots & "ppt_cum" %in% plots) {
    return(gridExtra::grid.arrange(ppt_inst, ppt_cum))
  }

  # instantaneous precipitation plot
  if ("ppt_inst" %in% plots) {
    return(ppt_inst)
  }

  # cumulative precipitation plot
  if ("ppt_cum" %in% plots) {
    return(ppt_cum)
  }
}
