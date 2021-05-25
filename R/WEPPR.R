#' A package for WEPP simulation and emulation
#'
#' The water erosion prediction project (WEPP) is a computer model developed by
#' the United States Department of Agriculture to simulate erosion processes.
#'
#' This package provides an R interface for reading and writing WEPP input
#' files (cli, slp, sol, run) and reading WEPP output files (env, wb, yld).
#'
#' Eventually it will also have functionality to run WEPP, at least on Linux
#' machines. The idea is to allow the user to programmatically run a set of WEPP
#' simulations by modifying the input in R, writing that input to files, running
#' WEPP, and then reading the resulting output files into R. Thus, this will
#' enable advanced exploration of the WEPP model.
#'
#' @docType package
#' @source <https://www.ars.usda.gov/midwest-area/west-lafayette-in/national-soil-erosion-research/docs/wepp/research/>
#' @name WEPPR
#'
NULL
