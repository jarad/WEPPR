#' Read a WEPP run file in DEP format
#'
#' This is a temporary function until we determine how to deal with run files
#' globally. This function will read in a run file in the daily erosion project
#' (DEP) format. It will only record the input and ouput files.
#'
#' @param file A path to the file.
#' @return A character vector with the following named elements:
#' \describe{
#'   \item{man}{management input filename}
#'   \item{slp}{slope input filename}
#'   \item{sol}{soil input filename}
#'   \item{cli}{climate input filename}
#'   \item{wb}{wb output filename}
#'   \item{env}{env output filename}
#'   \item{yld}{yield output filename}
#' }
#' @export
#'
read_dep_run <- function(file) {
  lines <- readLines(file, 28)[c(22,23,25,24,10,16,21)]
  names(lines) <- c("man","slp","sol","cli","wb","env","yld")

  return(lines)
}

#' Writes a run file in the DEP format
#'
#' This is a temporary function until we determine how to deal with run files
#' globally. This will write a run file using all the same options used for the
#' daily erosion project but replacing the input/output filenames with those
#' passed to the function.
#'
#' @param files A named character vector with names man, slp, sol, cli, wb, env,
#' and yld
#' @param run_filename A character for the name of the run file.
#'
#' @export
#'
write_dep_run <- function(files, run_filename) {
  run_file <- system.file("extdata", "071000090603_2.run", package="WEPPR")
  lines <- readLines(run_file)

  lines[c(22,23,25,24,10,16,21)] <- files
  writeLines(lines, run_filename)
}
