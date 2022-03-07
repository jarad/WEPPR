#' Create a Slope Class
#'
#' Creates a slope class from a slope (*.slp) file
#'
#' @param file A path to the slp file
#' @export
#' @examples
#' example
#' slp_class <- new_Slope(slp_data)
#' validate_Slope(slp_class)
#' plot(slp_class)
#' linearize_slp(slp_class)

# constructor
new_Slope <- function(x = data.frame()) {
  stopifnot(is.data.frame(x))
  structure(x, class = "Slope")
}

# validator
validate_Slope <- function(slp_class) {
  df <- tibble(n = slp_class$n,
               slope = slp_class$slope)

  # find rows where difference in n is 1 and difference in slope is zero
  diff_sum <- df %>%
    group_by(n) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(diff = slope - lag(slope), diff_n = n - lag(n)) %>%
    filter(diff_n == 1) %>%
    summarize(valid = sum(diff)) %>%
    pull()

  # validation method is summing the rows and checking if it equates to zero
  if (diff_sum != 0) {
    stop("Slope does not match across OFE", call. = FALSE)
  }
}


# argument to plot elevation or slope points or both (default elevation)
plot.Slope <- function(slp_class,) {
  # form data frame
  df <- tibble(n = slp_class$n,
               distance = slp_class$distance,
               slope = slp_class$slope)

  # calculate cumulative distance
  distance <- df %>%
    group_by(n) %>%
    mutate(diff =  distance - lag(distance)) %>%
    ungroup(n) %>%
    mutate(cumsum = cumsum(ifelse(is.na(diff), 0, diff))) %>%
    select(cumsum) %>%
    pull()

  # get slope points
  slp_data <- slp_df_ori$slope

  g <- ggplot(data = NULL, aes(distance, slp_data)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(x = "distance", y = "slope", title = "Slope (linear interpolation)")

  g
}

linearize_slp <- function(slp_df_ori) {
  remove_trailing_pattern <- '^(\\.\\d*?[1-9])0+$'

  df <- tibble(n = slp_class$n,
               distance = slp_class$distance,
               slope = slp_class$slope)

  slp_df <- df %>%
    group_by(n) %>%
    mutate(slp_distance = max(distance)) %>%
    ungroup() %>%
    select(slope, slp_distance) %>%
    mutate(ID = paste(
      "slp_slope",
      sep = "_",
      str_remove(round(1:n() / n(), digits = 3), remove_trailing_pattern)
    )) %>%
    pivot_wider(names_from = ID, values_from = slope) %>%
    summarise_all(sum, na.rm = T)

  slp_df
}
