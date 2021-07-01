#' Runs the WEPP model
#'
#' An R interface to run the Water Erosion Prediction Project (WEPP) model.
#'
#' @param file A path to the file.
#' @return A data frame containing the following elements:
#' \describe{
#'   \item{file}{character, file name}
#'   \item{file_type}{character, type of file eg. climate, environment,..}
#'   \item{type}{character, input/output}
#'   \item{md5sum}{character, hash of the file}
#'   \item{id}{character, conconated hash of hash of all the file names}
#' }
#' @export
#'
run_wepp <- function(file) {

  #check for correct file extension and Operating system
  stopifnot("Please pass a file with correct extension" =
              tools::file_ext(file) == "run")
  stopifnot("Please run the function on Linux Operating System" =
              WEPPR::is_linux() == "TRUE")

  #checks for the binary file
  stopifnot("WEPP executable not found"= is_wepp_available() == "TRUE")
  binary.file <- list.files(getwd(), pattern = "wepp$")
  stopifnot(
    "Please ensure wepp executable file is present in the working directory"=
    (WEPPR::hash(binary.file, file=TRUE) == "aab6591e5ae146ee61eb7cf162aef605"))

  runfile <- readLines(file, 28)[c(22,23,25,24,10,16,21)]
  names(runfile) <- c("man","slp","sol","cli","wb","env","yld")
  input_files <- runfile[c("man","slp","sol","cli")]
  output_files <- runfile[c("wb","env","yld")]

  #copies the input file and binary file to working directory
  current.folder <- getwd()
  working.folder <- tempdir()
  file.copy(c(input_files,binary.file,file), working.folder)
  setwd(working.folder)

  #runs the binary files using the input files and
  #copies the output files to the working directory
  system(paste(command = 'wepp<',file,' > screen.txt'), wait = TRUE)
  file.copy(c(output_files,"screen.txt"), current.folder)
  files <- c(output_files, input_files)

  #create the columns for data frame to be returned
  file.name <- names(files)
  file.type <- ifelse(file.name %in% c("cli","man","slp","sol"),"input",
                      ifelse(file.name %in% c("env","yld","wb","txt"),
                             "output","unknown"))
  lookup <-read.table(header = TRUE,
                      stringsAsFactors = FALSE,
                      text="name type
                            cli climate
                            env environment
                            man management
                            run run
                            slp slope
                            sol soil
                            wb  waterbalance
                            yld yield")
  type <- with(lookup, type[match(file.name,name)])
  uniquehash <- as.vector(WEPPR::hash(files, file = TRUE))
  id <- digest::digest(paste(uniquehash, collapse = ''),algo= "md5")
  run.out <- data.frame("file_name" = as.vector(files),
                        "file_type" = type,
                        "type" = file.type,
                        "md5sum" = uniquehash)
  run.out$Id <- id
  return(run.out)
}
