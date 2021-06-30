#' Checks for Linux OS
#'
#' @return returns TRUE if the OS is Linux and FALSE otherwise
#'
is_linux <- function() {
  isTRUE(grepl("linux",
               Sys.info()['sysname'],
               ignore.case = TRUE))
}
