#' Create a Soil Class
#'
#' The soil class constructor appends the Soil class to the Soil object
#' The constructor validates the soil data frame, and the Soil class provides
#' functionality to plot the soil features and to interpolate the Soil data
#' @name Soil
#' @param sol A soil dataframe
#' @return Soil object with class \code{Soil} and \code{data.frame}
#' @rdname Soil
#' @examples
#' fpath_sol <- system.file("extdata", "071000090603_2.sol", package="WEPPR")
#' sol <- read_sol(fpath_sol)
new_Soil <- function(sol = data.frame()) {
  stopifnot(is.data.frame(sol))

  ## validate all numeric columns are positive
  neg_cnt <- sol %>%
    filter(if_any(where(is.numeric), ~ .x < 0)) %>%
    nrow()

  if (neg_cnt != 0) {
    stop("Negative values exist in soil data", call. = FALSE)
  }

  class(sol) <- append("Soil", class(sol))

  return(structure(sol))
}


#' Merge Slope and Soil object to provide Soil object with distance for plotting
#'
#' @param layer the layer column in sol object
#' @param distance the distance column in sol object
#' @return slp_sol object with class Soil and tibble
#' @seealso \code{\link{plot.Soil}}
#' @export
#' @examples
#' fpath_slp <- system.file("extdata", "071000090603_2.slp", package="WEPPR")
#' fpath_sol <- system.file("extdata", "071000090603_2.sol", package="WEPPR")
#' slp <- read_slp(fpath_slp)
#' sol <- read_sol(fpath_sol)
#' slp_sol <- merge_slp_sol(slp, sol)
#' slp_sol
#'
merge_slp_sol <- function(slp, sol) {
  # filter numeric values
  filter_num <- sol %>%
    as_tibble() %>%
    select_if(is.numeric) %>%
    select(-Ke,-flag_Kb_Ke,-unknown)

  slp_layer <- slp %>%
    group_by(n) %>%
    filter(distance == max(distance)) %>%
    ungroup() %>%
    mutate(distance = round(distance, 3)) %>%
    select(-p) %>%
    rename(layer  = n)

  # join both data frames
  slp_sol <- left_join(filter_num, slp_layer, by = "layer")

  class(slp_sol) <- append("Soil", class(slp_sol))

  return(slp_sol)
}



#' Calculate cumulative distance across groups used for linearizing the soil file
#'
#' @param layer the layer column in sol object
#' @param distance the distance column in sol object
#' @return a numeric vector containing cumulative distance
#' @seealso \code{\link{plot.Soil}}
#' @examples
#' calculate_cum_dist(sol$layer, sol$distance)
#'
calculate_cum_dist <- function(layer, distance) {
  rep <- as.vector(table(layer))
  unique_dist <- cumsum(unique(distance))

  cum_dist <- c()
  for (i in 1:length(rep)) {
    cum_dist <- c(cum_dist, rep(unique_dist[i], rep[i]))
  }

  return(cum_dist)
}

#' Calculate xmin vector for geom_rect
#'
#' @param layer the layer column in sol object
#' @param distance the distance column in sol object
#' @return a numeric vector xmin needed to plot geom_rect
#' @seealso \code{\link{plot.Soil}}
#' @examples
#' get_xmin(sol$layer, sol$distance)
#'
get_xmin <- function(layer, distance) {
  rep <- as.vector(table(layer))
  uniq_dist <- cumsum(unique(distance))
  dist <- c(0, uniq_dist[-length(uniq_dist)])

  xmin <- c()
  for (i in 1:length(rep)) {
    xmin <- c(xmin, rep(dist[i], rep[i]))
  }

  return(xmin)
}


#' Plots the features in the Soil object
#'
#' @param slp_sol a Soil object merged with Slope data
#' @return multiple ggplot objects which are plots of soil features
#' @export
#' @examples
#' slp_sol <- merge_slp_sol(slp, sol)
#' plot(slp_sol)
#'
plot.Soil <- function(slp_sol) {
  if (!require(ggplot2))
    stop("You must install the 'ggplot2' package.")

  if (!require(ggpubr))
    stop("You must install the 'ggpubr' package.")

  if (!require(tidyr))
    stop("You must install the 'gridExtra' package.")

  # pivot data to bring features into one column, and its values in another
  slp_sol_pivot <- slp_sol %>%
    group_by(layer) %>%
    mutate(diff = solthk - lag(solthk, default = 0)) %>%
    ungroup() %>%
    select(-solthk) %>%
    pivot_longer(c(salb:rfg), names_to = "type")

  # loop through each type and plot
  combined_plt <-
    ggarrange(plotlist = lapply(split(slp_sol_pivot, slp_sol_pivot$type), function(x) {
      x %>%
        group_by(layer) %>%
        mutate(y_max = cumsum(diff)) %>%
        mutate(y_min = lag(y_max, default = 0), .before = y_max) %>%
        ungroup() %>%
        mutate(x_max = calculate_cum_dist(layer, distance)) %>%
        mutate(x_min = get_xmin(layer, distance), .before = x_max) %>%
        ggplot(aes(
          xmin = x_min,
          xmax = x_max,
          ymin = -y_min,
          ymax = -y_max
        )) +
        geom_rect(aes(fill = value), colour = "grey20") +
        labs(x = "distance", y = "soil thickness (mm)") +
        ggtitle(x$type[1])
    }))

  return(combined_plt)
}


#' Write the soil data file
#'
#' @param sol A \code{sol} object.
#' @param path Path to write to.
#' @export
#' @examples
#' #' fpath_sol <- system.file("extdata", "071000090603_2.sol", package="WEPPR")
#' sol <- read_sol(fpath_sol)
#' write_sol(sol, "example.sol")
#'
write_sol_file <- function(sol, path) {
  write_sol(sol, path)
}
