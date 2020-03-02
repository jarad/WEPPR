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

  run <- run_variables
  run$value <- read.csv(file,
                        header = FALSE,
                        stringsAsFactors = FALSE,
                        col.names = "value")

  return(run[,c("name","value","question")])
}


#' Run option variables
#'
#' Contains all possible run option variables with the associated question
#' that is asked.
#'
"run_variables"
