#' Will check for WEPP executable
#'
#' @return if WEPP executable is in the path, returns TRUE otherwise returns FALSE
#'
is_wepp_available <- function() {
  if (!is_linux()) {
    message("WEPP only available on Linux OS")
    return(FALSE)
  }

  path <- Sys.which("wepp")

  if (nchar(path) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
