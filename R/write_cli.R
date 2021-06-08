#' Write a WEPP climate file
#'
#' Writes a Water Erosion Prediction Project (WEPP) climate (*.cli) file.
#' This file contains climate information.
#'
#' @param cli A \code{cli} object.
#' @param path Path to write to.
#'
write_cli <- function(cli, path) {
  # cligen version
  write(paste(" ", cli$cligen_version), file = path)

  # cligen settings
  write(paste("  ", cli$cligen_settings, collapse = " "),
        file = path, append = TRUE)

  # station
  write(paste("  ", cli$station, collapse = " "),
        file = path, append = TRUE)

  # location
  write(paste(" ", "Latitude Longitude Elevation (m) Obs. Years   Beginning year  Years simulated"),
        file = path, append = TRUE)
  write(paste(" ", cli$location, collapse = " "),
        file = path, append = TRUE)

  # average max temp
  write(paste(" ", "Observed monthly ave max temperature (C)"),
        file = path, append = TRUE)
  write(paste(" ", cli$averages$obmaxt, collapse = " "),
        file = path, append = TRUE)

  # average min temp
  write(paste(" ", "Observed monthly ave min temperature (C)"),
        file = path, append = TRUE)
  write(paste(" ", cli$averages$obmint, collapse = " "),
        file = path, append = TRUE)

  # average solar radiation
  write(paste(" ", "Observed monthly ave solar radiation (Langleys/day)"),
        file = path, append = TRUE)
  write(paste(" ", cli$averages$radave, collapse = " "),
        file = path, append = TRUE)

  # average precip
  write(paste(" ", "Observed monthly ave precipitation (mm)"),
        file = path, append = TRUE)
  write(paste(" ", cli$averages$obrain, collapse = " "),
        file = path, append = TRUE)


  # precip
  write(paste(" ",
              "da mo year  prcp  dur   tp     ip  tmax  tmin  rad  w-vl w-dir  tdew"),
        file = path, append = TRUE)
  write(paste(" ",
              "            (mm)  (h)               (C)   (C) (l/d) (m/s)(Deg)   (C)"),
        file = path, append = TRUE)

  precip = cli$precip
  brkpnts = cli$breakpoints
  nbr = precip$nbrkpt
  N = length(nbr)
  count = 0

  for (i in 1:N) {
    if (nbr[i] == 0) {
      # extract dates
      da <- lubridate::day(precip$date[i])
      mo <- lubridate::month(precip$date[i])
      year <- lubridate::year(precip$date[i])

      write(paste(" ", c(da, mo, year, precip[i,-ncol(precip)]), collapse = " "),
            file = path, append = TRUE)
    } else {
      # print out last precip row
      da <- lubridate::day(precip$date[i])
      mo <- lubridate::month(precip$date[i])
      year <- lubridate::year(precip$date[i])

      write(paste(" ", c(da, mo, year, precip[i,-ncol(precip)]), collapse = " "),
            file = path, append = TRUE)

      for (j in 1:nbr[i]) {
        write(paste(" ", brkpnts[j+count, -ncol(brkpnts)], collapse = " "),
              file = path, append = TRUE)
      }
      count <- count+nbr[i]
    }
  }
}
