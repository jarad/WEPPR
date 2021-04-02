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

  crops = c()
  crop_type = c()
  date = c()
  OFE = c()
  yield = c()
  unit = c()
  year = c()

  blank_line = 0
  line = 7
  count = 1

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

      crop_type[count] = as.numeric(tmp[6])
      date[count] = as.numeric(tmp[9])
      OFE[count] = as.numeric(tmp[13])
      yield[count] = as.numeric(tmp[18])
      unit[count] = tmp[19]
      year[count] = as.numeric(tmp[21])

      count = count + 1
    } else {
      blank_line = blank_line + 1
    }
    line = line + 1
  }

  crops = data.frame(crops, row.names = NULL)
  colnames(crops) = c('id', 'name')

  yields = data.frame(crop_type, date, OFE, yield, unit, year)

  return(list(crops, yields))
}
