#' Create a Soil Class
#'
#' The soil class constructor appends the Soil class to the Soil object
#' The constructor validates the soil data frame, and the Soil class provides
#' functionality to plot the soil features and to linearize the Soil data
#' @name Soil
#' @param sol A soil dataframe
#' @return Soil object with class \code{Soil} and \code{data.frame}
#' @export
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


#' Calculate the duplicate count for each row based on solthk bin for linearizing
#' soil file
#'
#' @param sol A soil object
#' @return A numeric vector of duplicate counts
#' @seealso \code{\link{linearize_sol}}
#' @export
#' @examples
#' dup_counts <- get_dup_count(sol)
#' dup_counts
#'
get_dup_count <- function(sol) {
  dup <- c()
  j = 0

  # loop through each value
  for (i in 1:nrow(sol)) {
    d = 0

    # while it's greater than bin value, increment
    while (sol[i, ]$solthk > j) {
      d = d + 1
      j = j + 100
    }
    dup <- c(dup, d)
  }

  # calculate duplicates of the final bin (19th)
  left <- 19 - sum(dup)

  # add it to the final duplicate value
  dup[length(dup)] <- dup[length(dup)] + left

  return(dup)
}


#' Merge Slope and Soil object to provide Soil object with distance for plotting
#'
#' @param layer the layer column in sol object
#' @param distance the distance column in sol object
#' @return slp_sol object with class Soil and tibble
#' @seealso \code{\link{plot.Soil}}, \code{\link{linearize_sol}}
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


#' Linearize the soil data file
#'
#' Converts the soil file into one WEPP run (data frame with one row)
#'
#' @param slp_sol A Soil object combined with slope data
#' @seealso \code{\link{expand_slp}}
#' @return a one-row dataframe containing soil data
#' @export
#' @examples
#' slp_sol <- merge_slp_sol(slp, sol)
#' lin_sol <- linearize_sol(slp_sol)
#'
linearize_sol <- function(slp_sol) {

  norm_dist_cum <- slp_sol %>%
    summarize(cum_dist=cumsum(unique(distance))) %>%
    summarize(norm_cum_dist=cum_dist/max(cum_dist)) %>%
    pull()

  # create solthk bin vector
  bins <- tibble(solthk = cbind(seq(0, 1800, by = 100))) %>% pull()

  # filter numeric values
  filter_num <- sol %>%
    as_tibble() %>%
    select_if(is.numeric) %>%
    select(-Ke,-flag_Kb_Ke,-unknown)

  # duplicate rows based on count
  dups_sol <- filter_num %>%
    group_by(layer) %>%
    group_map( ~ get_dup_count(.x)) %>%
    unlist() %>%
    cbind(freq = ., filter_num) %>%
    mutate(freq = map(freq, seq_len)) %>%
    unnest(freq) %>%
    select(-freq)

  # linearize data
  lin_sol <- dups_sol %>%
    group_by(layer) %>%
    mutate(solthk_bin = bins, .before = sand) %>%
    ungroup() %>%
    mutate(id = row_number()) %>%
    group_by(layer) %>%
    pivot_wider(
      names_from = c(layer, id, solthk_bin),
      names_glue = "sol_{.value}_{round(norm_dist_cum[layer], 3)}_{solthk_bin}",
      values_from = c(salb:rfg)
    )

  return(lin_sol)
}


#' Calculate cumulative distance across groups used for linearizing the soil file
#'
#' @param layer the layer column in sol object
#' @param distance the distance column in sol object
#' @return a numeric vector containing cumulative distance
#' @seealso \code{\link{plot.Soil}}
#' @export
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
#' @export
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

  # pivot data to bring features into one column, and its values in another
  slp_sol_pivot <- slp_sol %>%
    group_by(layer) %>%
    mutate(diff = solthk - lag(solthk, default = 0)) %>%
    ungroup() %>%
    select(-solthk) %>%
    pivot_longer(c(salb:rfg), names_to = "type")

  # loop through each type and plot
  combined_plt <- ggarrange(
    plotlist = lapply(split(
      slp_sol_df_pivot, slp_sol_df_pivot$type
    ), function(x) {
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
        scale_fill_continuous(limit = c(0, 100),
                              low = "#56B1F7",
                              high = "#132B43") +
        labs(x = "distance", y = "soil thickness (mm)") +
        ggtitle(x$type[1])
    }),
    ncol = 2,
    nrow = 2
  )

  return(combined_plt)
}
