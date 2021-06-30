#' A package for WEPP simulation and emulation
#'
#' The water erosion prediction project (WEPP) is a computer model developed by
#' the United States Department of Agriculture to simulate erosion processes.
#'
#' This package provides an R interface for reading and writing WEPP input
#' files and reading WEPP output files. Initial development is focused on
#' reading/writing input/output files used in the Daily Erosion Project and
#' using their conventions. In particular, the package has functionality to
#' read/write the following input files (abbreviation)
#'
#'   - slope (slp),
#'   - soil properties (soil),
#'   - management (man),
#'   - climate/weather (cli), and
#'
#' and functionality to read the following output files (abbreviation)
#'
#'   - event output (env),
#'   - water balance (wb), and
#'   - yield/biomass (yld).
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
