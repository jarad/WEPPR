#' Read a WEPP management file
#'
#' Reads a Water Erosion Prediction Project (WEPP) plant/management (*.man) file.
#' This file contains plant/management information.
#'
#' @param file A path to the file.
#' @return A \code{man} \code{list} with management set-up
#' !!! needs more comments here !!!
#'   following elements:
#'   \describe{
#'     \item{manver}{}
#'     \item{iofe}{}
#'     \item{inyr}{}
#'     \item{ncrop}{}
#'     \item{crops}{}
#'     \item{nop}{}
#'     \item{operations}{}
#'     \item{nini}{}
#'     \item{ini}{}
#'     \item{nsurf}{}
#'     \item{surfeffects}{}
#'     \item{ncnt}{}
#'     \item{ndrain}{}
#'     \item{nmscen}{}
#'     \item{scens}{}
#'     \item{mantitle}{}
#'     \item{mandesc}{}
#'     \item{nwsofe}{}
#'     \item{inindx}{}
#'     \item{nrots}{}
#'     \item{nyears}{}
#'     \item{rotations}{}
#'   }
#' @export
#'
read_man <- function(file) {
  require("reticulate")

  py_function = py_run_file("./R/read_man.py", local = TRUE)$read_man

  res = py_function(file)

  return(res)
}
