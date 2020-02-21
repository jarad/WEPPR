#' Read a WEPP climate file
#'
#' Reads a Water Erosion Prediction Project (WEPP) climate (*.cli) file.
#' This file contains climate information.
#'
#' @param file A path to the file.
#' @return A list containing the following elements:
#' \describe{
#'   \item{cligen_version}{numeric, CLIGEN version, e.g. 4.30}
#'   \item{cligen_settings}{numeric, }
#'   \item{station}{character, name of station}
#'   \item{location}{numeric}
#'   \item{averages}{numeric, monthly average}
#'   \item{precip}{numeric, daily precipitation}
#'   \item{breakpoints}{numeric, (optional) sub-daily precipitation}
#' }
#'
#' @details
#' \code{cligen_settings} is a vector with the following named elements:
#' \describe{
#'   \item{itemp}{1 - continuous simulation, 2 - single storm}
#'   \item{ibrkpt}{0 - no breakpoints, 1 - breakpoints}
#'   \item{iwind}{0 - wind information exists, 1 - no wind information exists}
#' }
#'
#' \code{location} is a vector with the following named elements:
#' \describe{
#'   \item{deglat}{degrees latitude (+ is North, - is South)}
#'   \item{deglon}{degrees longitude (+ is East, - is West)}
#'   \item{elev}{station elevation (m)}
#'   \item{obsyrs}{weather station years of observation}
#'   \item{ibyear}{beginning year of CLIGEN simulation}
#'   \item{numyr}{number of climate years simulated and in file}
#' }
#'
#' \code{averages} is a \code{data.frame} with 12 rows and the following columns:
#' \describe{
#'   \item{month}{1 - January, 2 - February, etc}
#'   \item{obmaxt}{average maximum temperature (C) for the month}
#'   \item{obmint}{average minimum temperature (C) for the month}
#'   \item{radave}{average daily solar radiation (langleys)}
#'   \item{obrain}{average precipitation (mm)}
#' }
#' These averages do not match the averages calculated from \code{precip}.
#'
#' \code{precip} is a \code{data.frame} containing the following columns:
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
#'
#' \code{breakpoints} is a \code{data.frame} containing the following columns:
#' \describe{
#'   \item{da}{numeric, day of the month}
#'   \item{mo}{numeric, month}
#'   \item{year}{numeric, year}
#'   \item{timem}{time since midnight HH.MM}
#'   \item{pptcum}{cumulative precipitation (mm) since midnight}
#' }
#' @export
#' @source <https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/usersum.pdf>
#'
read_cli <- function(file, breakpoints = NULL) {

  cli <- read_cli_raw(file)

  if (cli$cligen_settings$ibrkpt) {
    bp              <- extract_breakpoints(cli$precip)
    cli$precip      <- bp$precip
    cli$breakpoints <- bp$breakpoints
  }

  # what else do we need to extract?
  # tmp = readLines(file)

  # return precip
  return(cli)
}




#' Read climate information
#'
#' Reads climate file header information including version, settings, station,
#' location, averages, and precipitation data that may or may not include
#' breakpoints.
#'
#' @rdname read_cli
#'
read_cli_raw <- function(file) {

  # CLIGEN simulation settings
  cligen_version <- read.table(file, header = FALSE, nrows = 1,
                              as.is = TRUE, col.names = "cligen_version")

  cligen_settings <- read.table(file, skip = 1, header = FALSE, nrows = 1,
                               as.is = TRUE, col.names = c("itemp",  # 1 - continuous, 2 - single storm
                                                           "ibrkpt", # 0 - no breakpoints, 1 - breakpoint data used
                                                           "iwind")) # 0 - wind information exists, 1 - no wind information exists

  station <- read.table(file, skip = 2, nrows = 1, header = FALSE, sep="")

  location <- read.table(file, skip = 4, nrows = 1, header = FALSE, sep="",
                         col.names = c("deglat","deglon","elev","obsyrs","ibyear","numyr"))


  # extract monthly averages
  averages <- data.frame(month = 1:12,
                        obmaxt = scan(file, skip =  6, n = 12),
                        obmint = scan(file, skip =  8, n = 12),
                        radave = scan(file, skip = 10, n = 12),
                        obrain = scan(file, skip = 12, n = 12))


  # extract daily weather
  headers <- read.table(file, skip = 13, header = FALSE, nrows = 1, as.is = TRUE)
  stopifnot(all(headers == c("da",  "mo", "year", "prcp", "dur",
                             "tp", "ip", "tmax", "tmin", "rad",
                             "w-vl", "w-dir", "tdew")))

  precip <- read.table(file,
                      skip = 15,
                      header = FALSE,
                      fill = TRUE)

  # the same column names are used regardless of whether the file has breakpoints,
  # but the column name should be different
  if (cligen_settings$ibrkpt) {
    colnames(precip) <- c("da", "mo", "year", "nbrkpt", "tmax",
                          "tmin", "rad", "w-vl", "w-dir", "tdew")
  } else{
    colnames(precip) <- headers
  }

  return(list(cligen_version  = cligen_version,
              cligen_settings = cligen_settings,
              station         = station,
              location        = location,
              averages        = averages,
              precip          = precip))
}



#' Extracts breakpoints data from a CLIGEN precipitation table
#'
#' CLIGEN climate files either have breakpoints that indicate rainfall on
#' sub-daily time frames or have daily summaries only. This function will read
#' a climate file that has breakpoints and return a list containing climate
#' file information including daily summaries as well as breakpoint information.
#'
#' @param precip_with_breakpoints data.frame containing precipitation data and breakpoints
#' @return A list containing two elements: precip and breakpoints
#' @rdname read_cli
#'
extract_breakpoints <- function(precip_with_breakpoints) {

  precip = na.omit(precip_with_breakpoints)

  # extract the rows containing NAs
  breakpoints <- precip_with_breakpoints[is.na(rowSums(precip_with_breakpoints)),1:2]
  names(breakpoints) <- c("timem",  # time after midnight (hours)
                          "pptcum") # cumulative precipitation (mm) at this time

  breakpoints <- cbind(precip[rep.int(1:nrow(precip), precip$nbrkpt),
                              c("da","mo","year")],
                       breakpoints)

  return(list(precip      = precip,
              breakpoints = breakpoints))
}


