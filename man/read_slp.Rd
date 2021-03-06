% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_slp.R
\name{read_slp}
\alias{read_slp}
\title{Read a WEPP slope file}
\usage{
read_slp(file)
}
\arguments{
\item{file}{A path to the file.}
}
\value{
A \code{slp} \code{data.frame} with attributes version control number,
  aspect of the profile (degrees from North) (azm), and
  representative profile width(m) fwidth, and the
  following columns:
  \describe{
    \item{n}{overland flow element (OFE) #}
    \item{p}{proportion of the total distance for this OFE}
    \item{slope}{vertical distance lost (m) per meter traveled}
    \item{distance}{proportion (p) times total distance (m)}
  }
}
\description{
Reads a Water Erosion Prediction Project (WEPP) slope (*.slp) file.
This file contains slope information assumed to be in nondimensional distances.
}
