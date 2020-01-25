#' Read a WEPP climate file
#'
#' Reads a Water Erosion Prediction Project (WEPP) climate (*.cli) file.
#' This file contains climate information.
#'
#' @param file A path to the file.
#' @return A \code{data.frame} containing the following columns:
#' \describe{
#'   \item{da}{numeric, day of the month}
#'   \item{mo}{numeric, month}
#'   \item{year}{numeric, year}
#'   \item{prcp}{numeric, daily precipitation amount (mm)}
#'   \item{dur}{numeric, duration of precipitation (hr)}
#'   \item{tp}{numeric, ratio of time to rainfall peak/rainfall duration}
#'   \item{ip}{numeric, ratio of maximum rainfall intensity/average rainfall intensity}
#'   \item{tmax}{numeric, daily temperature maximum (degrees C)}
#'   \item{tmin}{numeric, daily temperature minimum (degrees C)}
#'   \item{rad}{numeric, daily solar radiation (landleys/day)}
#'   \item{w-vl}{numeric, wind velocity (m/sec)}
#'   \item{w-dir}{numeric, wind direction (degrees from North)}
#'   \item{tdew}{numeric, dew point temperature (degrees C)}
#' }
#' @source <https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/usersum.pdf>
#'
read_cli <- function(file) {
  warning("This function is a work in progress.")

  # CLIGEN simulation settings
  cligen_version = read.table(file, header = FALSE, nrows = 1,
                              as.is = TRUE, col.names = "cligen_version")

  cligen_settings = read.table(file, skip = 1, header = FALSE, nrows = 1,
                               as.is = TRUE, col.names = c("itemp",  # 1 - continuous, 2 - single storm
                                                           "ibrkpt", # 0 - no breakpoints, 1 - breakpoint data used
                                                           "iwind")) # 0 - wind information exists, 1 - no wind information exists

  station = read.table(file, skip = 2, nrows = 1, header = FALSE, sep="")

  location = read.table(file, skip = 3, nrows = 1, header = FALSE, sep="")


  # extract monthly averages
  averages = data.frame(month = 1:12,
                        ave_max_temp = scan(file, skip =  6, n=12),
                        ave_min_temp = scan(file, skip =  8, n=12),
                        ave_solar    = scan(file, skip = 10, n=12),
                        ave_prec     = scan(file, skip = 12, n=12))


  # extract daily weather
  headers = read.table(file, skip = 13, header = FALSE, nrows = 1, as.is = TRUE)

  precip = read.table(file,
                   skip = 15,
                   header = FALSE,
                   fill = TRUE)

  colnames(precip) = headers

  # what else do we need to extract?
  # tmp = readLines(file)

  # return precip
  return(list(cligen_version  = cligen_version,
              cligen_settings = cligen_settings,
              station         = station,
              location        = location,
              averages        = averages,
              precip          = precip))
}
