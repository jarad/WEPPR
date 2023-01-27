#' Read a WEPP slope file
#'
#' Reads a Water Erosion Prediction Project (WEPP) slope (*.slp) file.
#' This file contains slope information assumed to be in nondimensional distances.
#'
#' @param file A path to the file.
#' @return A \code{Slope} object with two classes - \code{data.frame} and \code{Slope}
#' The data frame class has the attributes version control number,
#'   aspect of the profile (degrees from North) (azm), and
#'   representative profile width(m) fwidth, and the
#'   following columns:
#'   \describe{
#'     \item{n}{overland flow element (OFE) #}
#'     \item{p}{proportion of the total distance for this OFE}
#'     \item{slope}{vertical distance lost (m) per meter traveled}
#'     \item{distance}{proportion (p) times total distance (m)}
#'   }
#' @export
#'
read_slp <- function(file) {
  if (!require(dplyr))
    stop("You must install the 'dplyr' package.")

  d <- readLines(file)

  version <- d[1]

  i <- 2 # line number
  while (substr(d[i], 1, 1) == "#")
    i <- i + 1

  nelem <- as.numeric(d[i]) # number of overland flow elements (OFEs)

  i      <- i+1
  tmp    <- as.numeric(strsplit(d[i], split = " ")[[1]])
  azm    <- tmp[1] # aspect of the profile (degrees from North)
  fwidth <- tmp[2] # representative profile width (m)

  OFEs <- list()
  for (n in 1:nelem) {
    i      <- i + 1
    tmp    <- as.numeric(strsplit(d[i], split = " ")[[1]])
    nsplts <- tmp[1] # number of splits
    slplen <- tmp[2] # length of OFE (m)

    i     <- i+1
    tmp   <- as.numeric(gsub(",","",strsplit(d[i], split = c(" ",", "))[[1]][-1]))
    p     <- tmp[seq(1,2*nsplts,by=2)]
    slope <- tmp[seq(2,2*nsplts,by=2)]

    OFEs[[n]] <- data.frame(n = n, p = p, slope = slope, distance = p*slplen)
  }

  OFEs <- do.call(rbind, OFEs)

  attr(OFEs, "version") <- version
  attr(OFEs, "azm")     <- azm
  attr(OFEs, "fwidth")  <- fwidth

  slp <- new_Slope(OFEs)

  return(slp)
}
