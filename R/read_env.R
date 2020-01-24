#' Read a WEPP environment file
#'
#' Reads a Water Erosion Prediction Project (WEPP) environment (*.env) file.
#' This file contains output from a WEPP run
#'
#' @param file A path to the file.
#' @export
#'
read_env <- function(file) {
  headers = read.table(file, skip = 1, header = F, nrows = 1, as.is = TRUE)

  env = read.table(file,
           skip = 3,
           header = FALSE)

  colnames(env) = headers

  env
}
