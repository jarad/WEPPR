% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Slope.R
\name{remove_slp_transitions}
\alias{remove_slp_transitions}
\title{Remove first row for each new OFE in a Slope object or Soil object}
\usage{
remove_slp_transitions(slp)
}
\arguments{
\item{slp}{A Slope object}
}
\value{
A modified Slope object with the first row of each new OFE removed.
}
\description{
In calculating slope and elevation, it is convenient if the Slope object
does not have the first row for each new OFE. This function will remove that
first row.
}
\examples{
slp <- read_slp(system.file("extdata", "071000090603_2.slp", package="WEPPR"))
remove_slp_transitions(slp)

remove_slp_transitions(data.frame(n = rep(1:3, times = c(3,4,5)))
}
\seealso{
\code{\link{plot.Slope}}, \code{\link{expand_slp}}, \code{\link{integrate_slp}}
}
