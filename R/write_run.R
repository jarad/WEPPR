#' Write a WEPP run settings file
#'
#' Writes a Water Erosion Prediction Project (WEPP) run settings (*.run) file.
#' This file contains run settings.
#'
#' @param run A \code{run} object.
#' @param path Path to write to.
#' @export
#'
write_run <- function(run, path) {
  write(run[[1]], file = path)
  for (i in 2:length(run))
    write(run[[i]], file = path, append = TRUE)
}
