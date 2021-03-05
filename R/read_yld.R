#' Read a WEPP yield file
#'
#' Reads a Water Erosion Prediction Project (WEPP) yield (*.yld) file.
#' This file contains yield information.
#'
#' @param file A path to the file.
#' @export
#'
read_yld <- function(file) {
  d <- readLines(file)

  yields= c()
  crops = c()

  blank_line = 0
  line = 7


  while (blank_line < 1) {
    tmp = strsplit(d[line], split = ' ')[[1]]

    if (length(tmp) >= 4) {
      crop_id = as.numeric(tmp[6])
      crop_type = tmp[8]

      crops = rbind(crops, c(crop_id, crop_type))
    } else {
      blank_line = blank_line + 1
    }
    line = line + 1
  }

  # from the previous loop
  line = line + 2

  while (blank_line < 2) {
    tmp = strsplit(d[line], split = ' ')[[1]]

    # assure that NOT a blank line
    if (length(tmp) >= 12) {
      blank_line = 0

      crop_type = as.numeric(tmp[6])
      date = as.numeric(tmp[9])
      OFE = as.numeric(tmp[13])
      yield = as.numeric(tmp[18])
      unit = tmp[19]
      year = as.numeric(tmp[21])

      row = c(crop_type, date, OFE, yield, unit, year)
      yields = rbind(yields, row)
    } else {
      blank_line = blank_line + 1
    }
    line = line + 1
  }

  crops = data.frame(crops, row.names = NULL)
  colnames(crops) = c('id', 'name')

  yields = data.frame(yields, row.names = NULL)
  colnames(yields) = c('crop_type', 'date', 'OFE', 'yield', 'unit', 'year')

  return(list(crops, yields))

  # i'm ignoring here the average yield harvest here!!!!
}
