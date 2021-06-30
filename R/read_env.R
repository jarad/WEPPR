#' Read a WEPP environment file
#'
#' Reads a Water Erosion Prediction Project (WEPP) environment (*.env) file.
#' This file contains environmental output from
#'
#' @param file A path to the file.
#' @param start.year First year of simulation. (Default is 2006 corresponding to DEP simulations.)
#' @return A \code{env} \code{data.frame} with the following columns:
#' \describe{
#'   \item{day}{Numeric, day of the month}
#'   \item{mo}{Numeric, day of the month}
#'   \item{year}{Numeric, day of the month}
#'   \item{Precp}{Numeric, day of the month}
#'   \item{Runoff}{Numeric, day of the month}
#'   \item{IR-det}{Numeric, day of the month}
#'   \item{Av-det}{Numeric, day of the month}
#'   \item{Mx-det}{Numeric, day of the month}
#'   \item{Point}{Numeric, day of the month}
#'   \item{Av-dep}{Numeric, day of the month}
#'   \item{Max-dep}{Numeric, day of the month}
#'   \item{Point}{Numeric, day of the month}
#'   \item{Sed.Del}{Numeric, sediment delivery ratio?}
#'   \item{ER}{Numeric, enrichment ratio?}
#' }
#' @export
#'
read_env <- function(file, start.year=2006) {
  headers = read.table(file, skip = 1, header = F, nrows = 1, as.is = TRUE)

  units = read.table(file, skip = 1, header = TRUE, nrows = 1, as.is = TRUE)

  env = read.table(file,
           skip = 3,
           header = FALSE)

  colnames(env) = headers
  attr(env, 'units') = units

  env$Date = as.Date(paste(start.year+env$year-1, env$mo, env$da, sep = '-'))

  class(env) <- append(class(env), "env")
  return(env[, -c(1:3)])
}
