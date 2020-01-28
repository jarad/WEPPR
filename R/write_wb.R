#' Write a WEPP wb file
#'
#' Writes a Water Erosion Prediction Project (WEPP) soil water balance (*.wb) file.
#' This file contains soil water balance information.
#'
#' @param wb A \code{wb} object.
#' @param path Path to write to.
#'
write_wb <- function(wb, path) {
  write.table(wb, file = path,
              col.names = FALSE)
}
