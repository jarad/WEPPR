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


#' Create a run object
#'
#' Interactively create a run object by asking questions in exactly the same
#' order as the WEPP interactive system. Ideally, you only need to go through
#' this process once and then you can modify the run object as needed, e.g. to
#' change file names.
#'
#' This run object specifies the type of WEPP simulation to perform as well as
#' names for input/output files.
#'
#' @return A data frame containing two columns
#' \describe{
#'   \item{name}{names of variables}
#'   \item{value}{the value for that variable}
#' }
#'
#' @export
#'
create_run <- function() {
  run <- data.frame(name = character(),
                    value = character())

  # questions <- merge(run_variables, wepp_run_questions,
  #                    by = "question", all.y = TRUE, sort = FALSE)
  questions <- wepp_run_questions

  line     <- 1
  while (line > 0) {
    question <- questions$question[line]
    answers <- questions$answer[questions$question == question]
    answer <- NULL

    while(!(answer %in% answers)) {
      answer <- readline(questions$question[line])

      if (answers == "text")
        answers <- answer # hack to ensure text answers are automatically accepted
    }

    question <- questions$question

    run <- cbind(run, tmp)
  }

  return(run)
}


#' Run option variables
#'
#' Contains all possible run option variables with the associated question
#' that is asked.
#'
"run_variables"
