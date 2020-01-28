#' Read a WEPP run file
#'
#' Reads a Water Erosion Prediction Project (WEPP) run (*.run) file.
#' This file contains run settings information.
#'
#' @param file A path to the file.
#' @return A list with the following named elements:
#' \describe{
#'   \item{wb_file}{character, filename for wb output file}
#'   \item{env_file}{character, filename for env output file}
#'   \item{yld_file}{character, filename for yld output file}
#'   \item{man_file}{character, filename for management input file}
#'   \item{slp_file}{character, filename for slope input file}
#'   \item{sol_file}{character, filename for soil input file}
#' }
#' @export
#'
read_run <- function(file) {
  warning("This function is not yet implemented.")

  d = readLines(file)

  list(
    V1 = d[1],
    V2 = d[2],
    V3 = d[3],
    V4 = d[4],
    V5 = d[5],
    V6 = d[6],
    V7 = d[7],
    V8 = d[8],
    V9 = d[9],
    wb_file = d[10],
    V11 = d[11],
    V12 = d[12],
    V13 = d[13],
    V14 = d[14],
    V15 = d[15],
    env_file = d[16],
    V17 = d[17],
    V18 = d[18],
    V19 = d[19],
    V20 = d[20],
    yld_file = d[21],
    man_file = d[22],
    slp_file = d[23],
    V21 = d[21],
    sol_file = d[25],
    V26 = d[26],
    V27 = d[27],
    V28 = d[28]
  )
}
