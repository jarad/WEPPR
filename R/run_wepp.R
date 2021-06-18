#' Runs the WEPP model
#'
#' An R interface to run the Water Erosion Prediction Project (WEPP) model.
#'
#' @param man A \code{man} object representing land management
#' @param run A \code{run} object representing run settings
#' @param slp A \code{slp} object representing slope
#' @param sol A \code{sol} object representing sol
#' @param path A path where files should be written. If null, files are written
#'   to a temporary directory and that directory is reported.
#'
run_wepp <- function(file="071000090603_2.run") {

  stopifnot("Please pass a file with correct extension" = tools::file_ext(file) == "run")
  stopifnot("Please run the function on Linux Operating System" = (Sys.info()[['sysname']] == "Linux"))

  binary.file <- list.files(getwd(), pattern = "wepp$")
  stopifnot("Please ensure wepp executable file is present in the working directory"= (tools::md5sum(binary.file) == "aab6591e5ae146ee61eb7cf162aef605"))


  current.folder <- getwd()
  new.folder <- tempdir()
  pattern1 = c(".cli",".man",".slp",".sol",".run$","wepp$")
  list.of.files <- list.files(current.folder,
                              pattern = paste0(pattern1, collapse = "|"))
  file.copy(list.of.files, new.folder)
  list.files(new.folder)
  setwd(new.folder)

  system(paste(command = './wepp<',file,' > screen.txt'), wait = TRUE)
  system(command='tail screen.txt')
  output.pattern = c(".env",".yld",".wb",".txt")
  list.of.files.output <- list.files(getwd(),pattern = paste0(output.pattern, collapse = "|"))
  file.copy(list.of.files.output, current.folder)


  n <- max(length(list.of.files),length(list.of.files.output))
  length(list.of.files) <- n
  length(list.of.files.output) <- n
  list.of.files.output[is.na(list.of.files.output)] <- ""
  run.out <- data.frame("Input_files"=list.of.files, "Output_files"=list.of.files.output)
  return(run.out)
}
