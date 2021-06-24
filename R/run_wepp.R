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
run_wepp <- function(file) {

  #check for correct file extension and Operating system
  stopifnot("Please pass a file with correct extension" =
              tools::file_ext(file) == "run")
  stopifnot("Please run the function on Linux Operating System" =
              (Sys.info()[['sysname']] == "Linux"))

  #checks for the binary file
  binary.file <- list.files(getwd(), pattern = "wepp$")
  stopifnot("Please ensure wepp executable file is present in the working directory"= (tools::md5sum(binary.file) == "aab6591e5ae146ee61eb7cf162aef605"))

  runfile <- read.table(file, header = FALSE,
                        stringsAsFactors = FALSE, col.names = "value")
  input_files <- runfile[c(22:25),1]
  output_files <- runfile[c(10,16,21),1]

  #copies the input file and binary file to working directory
  current.folder <- getwd()
  working.folder <- tempdir()
  file.copy(c(input_files,binary.file,file), working.folder)
  setwd(working.folder)

  #runs the binary files using the input files and
  #copies the output files to the working directory
  system(paste(command = './wepp<',file,' > screen.txt'), wait = TRUE)
  file.copy(c(output_files,"screen.txt"), current.folder)
  files <- c(output_files, input_files,file)

  #create the columns for data frame to be returned
  file.extension <- tools::file_ext(files)
  file.type <- ifelse(file.extension %in% c("cli","man","slp","sol","run","wepp"),
                      "input",
                      ifelse(file.extension %in% c("env","yld","wb","txt"),
                             "output",""))
  lookup <-read.table(header = TRUE,
                      stringsAsFactors = FALSE,
                      text="extension type
                            cli Climate
                            env Environment
                            man Management
                            run Run
                            slp Slope
                            sol Soil
                            wb  Water_balance
                            yld Yield")
  type <- with(lookup, type[match(file.extension,extension)])
  uniquehash <- as.vector(tools::md5sum(files))
  id <- digest::digest(paste(uniquehash, collapse = ''),"md5")
  run.out <- data.frame("File_name" = files,
                        "File_extension" = file.extension,
                        "File_type" = type,
                        "Type" = file.type,
                        "md5sum" = uniquehash)
  run.out$Id <- id
  return(run.out)
}
