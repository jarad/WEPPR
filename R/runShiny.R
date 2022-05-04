#' Run the WEPPR shiny app
#'
#' @name runShiny
#' @export
runShiny <- function() {
  require(shiny)

  appDir <- system.file("shiny", package = "WEPPR")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `WEPPR`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
