#' Read a WEPP soil file
#'
#' Reads a Water Erosion Prediction Project (WEPP) soil (*.sol) file.
#' This file contains soil information.
#'
#' @param file A path to the file.
#' @return A \code{Soil} object with two classes - \code{Soil} and \code{data.frame}
#' @export
#' @examples
#' fpath_sol <- system.file("extdata", "071000090603_2.sol", package="WEPPR")
#' sol <- read_sol(fpath_sol)
#'
read_sol <- function(file) {
  d <- readLines(file)

  tmp <- as.numeric(strsplit(d[3], split = " ")[[1]])
  n_layers <- tmp[1]
  ksflag <- tmp[2]

  OFEs <- list()
  layers <- list()
  conductivity <- list()
  i <- 4
  for (n in 1:n_layers) {
    # line 1 for this soil type
    tmp <- scan(text=d[i], what='character', quiet=TRUE)

    nsl <- as.integer(tmp[3])
    OFEs[[n]] <- data.frame(layer  = n,
                           slid   = tmp[1],
                           texid  = tmp[2],
                           # nsl    = nsl,
                           salb   = as.numeric(tmp[4]),
                           sat    = as.numeric(tmp[5]),
                           ki     = as.numeric(tmp[6]),
                           kr     = as.numeric(tmp[7]),
                           shcrit = as.numeric(tmp[8]),
                           avke   = as.numeric(tmp[9]),
                           stringsAsFactors = FALSE)

    # following lines for this soil type
    layers[[n]] <- read.table(textConnection(d[i + c(1:nsl)]),
                             col.names = c("solthk","sand","clay","orgmat","cec","rfg"))
    layers[[n]]$layer <- n

    # soil conductivity lines
    conductivity[[n]] <- read.table(textConnection(d[i+nsl+1]),
                              col.names = c("Ke","flag_Kb_Ke", "unknown"))
    conductivity[[n]]$layer <- n

    i <- i + 2 + nsl
  }
  OFEs   <- do.call(rbind, OFEs)
  layers <- do.call(rbind, layers)
  conductivity <- do.call(rbind, conductivity)

  soil <- merge(OFEs, layers, by = "layer", all = TRUE)
  soil <- merge(soil, conductivity, by = "layer", all = TRUE)

  attr(soil, "datver") <- d[1]
  attr(soil, "solcom") <- d[2]
  attr(soil, "ksflag") <- ksflag

  sol <- new_Soil(soil)

  return(sol)
}
