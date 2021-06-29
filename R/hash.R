#' Compute md5sum hash for a file
#'
#' Currently just wraps tools::md5sum for convenience.
#'
#' Perhaps in the future it will also hash R objects perhaps by writing them to
#' a file and then hashing the file.
#'
#' @param files path and names of files to calculate md5sum
#'
#' @value md5sum of file
#'
#' @importFrom tools md5sum
#'
hash <- function(files) {
  return(tools::md5sum(files))
}
