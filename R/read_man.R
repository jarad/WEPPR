#' Read a WEPP management file
#'
#' Reads a Water Erosion Prediction Project (WEPP) plant/management (*.man) file.
#' This file contains plant/management information.
#'
#' This function uses reticulate package, which is compatible with all versions
#' of Python >= 2.7. Integration with NumPy is optional and requires NumPy >= 1.6.
#'
#' @param file A path to the file.
#' @return A \code{man} \code{list} with management set-up and
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
  if(!require(reticulate))
    stop("You must install the 'reticulate' package.")

  py_function = reticulate::py_run_file("./R/read_man.py", local = TRUE)$read_man

  res = reticulate::py_function(file)

  return(res)
}
