#' Read a WEPP wb file
#'
#' Reads a Water Erosion Prediction Project (WEPP) soil water balance (*.wb) file.
#' This file contains soil water balance information.
#'
#' @param file A path to the file.
#' @return A data.frame with the columns:
#' \describe{
#'   \item{ofe}{numeric, overland flow element}
#'   \item{jday}{numeric (1-366), julian day}
#'   \item{year}{numeric, YYYY}
#'   \item{precip}{numeric, amount of precipitation}
#'   \item{runoff}{numeric, amount of runoff}
#'   \item{sw}{numeric}
#'   \item{sw1}{numeric}
#'   \item{sw2}{numeric}
#'   \item{ep}{numeric}
#'   \item{es}{numeric}
#'   \item{er}{numeric}
#' }
#' @export
#'
read_wb <- function(file) {
  res <- read.table(file, header = TRUE)

  # make a new date column out of date and year columns
  res$date <- as.Date(paste(res$year, 01, 01, sep = '-' )) + res$jday - 1

  return(res[,-c(2:3)])
}
