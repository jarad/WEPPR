% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_cli.R
\name{read_cli}
\alias{read_cli}
\alias{read_cli_raw}
\alias{extract_breakpoints}
\title{Read a WEPP climate file}
\source{
\url{https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/usersum.pdf}
}
\usage{
read_cli(file, breakpoints = NULL)

read_cli_raw(file)

extract_breakpoints(precip_with_breakpoints)
}
\arguments{
\item{file}{A path to the file.}

\item{precip_with_breakpoints}{data.frame containing precipitation data and breakpoints}
}
\value{
A list containing the following elements:
\describe{
  \item{cligen_version}{numeric, CLIGEN version, e.g. 4.30}
  \item{cligen_settings}{numeric, }
  \item{station}{character, name of station}
  \item{location}{numeric}
  \item{averages}{numeric, monthly average}
  \item{precip}{numeric, daily precipitation}
  \item{breakpoints}{numeric, (optional) sub-daily precipitation}
}

A list containing two elements: precip and breakpoints
}
\description{
Reads a Water Erosion Prediction Project (WEPP) climate (*.cli) file.
This file contains climate information.

Reads climate file header information including version, settings, station,
location, averages, and precipitation data that may or may not include
breakpoints.

CLIGEN climate files either have breakpoints that indicate rainfall on
sub-daily time frames or have daily summaries only. This function will read
a climate file that has breakpoints and return a list containing climate
file information including daily summaries as well as breakpoint information.
}
\details{
\code{cligen_settings} is a vector with the following named elements:
\describe{
  \item{itemp}{1 - continuous simulation, 2 - single storm}
  \item{ibrkpt}{0 - no breakpoints, 1 - breakpoints}
  \item{iwind}{0 - wind information exists, 1 - no wind information exists}
}

\code{location} is a vector with the following named elements:
\describe{
  \item{deglat}{degrees latitude (+ is North, - is South)}
  \item{deglon}{degrees longitude (+ is East, - is West)}
  \item{elev}{station elevation (m)}
  \item{obsyrs}{weather station years of observation}
  \item{ibyear}{beginning year of CLIGEN simulation}
  \item{numyr}{number of climate years simulated and in file}
}

\code{averages} is a \code{data.frame} with 12 rows and the following columns:
\describe{
  \item{month}{1 - January, 2 - February, etc}
  \item{obmaxt}{average maximum temperature (C) for the month}
  \item{obmint}{average minimum temperature (C) for the month}
  \item{radave}{average daily solar radiation (langleys)}
  \item{obrain}{average precipitation (mm)}
}
These averages do not match the averages calculated from \code{precip}.

\code{precip} is a \code{data.frame} containing the following columns:
\describe{
  \item{da}{numeric, day of the month}
  \item{mo}{numeric, month}
  \item{year}{numeric, year}
  \item{prcp}{numeric, daily precipitation amount (mm)}
  \item{dur}{numeric, duration of precipitation (hr)}
  \item{tp}{numeric, ratio of time to rainfall peak/rainfall duration}
  \item{ip}{numeric, ratio of maximum rainfall intensity/average rainfall intensity}
  \item{tmax}{numeric, daily temperature maximum (degrees C)}
  \item{tmin}{numeric, daily temperature minimum (degrees C)}
  \item{rad}{numeric, daily solar radiation (landleys/day)}
  \item{w-vl}{numeric, wind velocity (m/sec)}
  \item{w-dir}{numeric, wind direction (degrees from North)}
  \item{tdew}{numeric, dew point temperature (degrees C)}
}

\code{breakpoints} is a \code{data.frame} containing the following columns:
\describe{
  \item{da}{numeric, day of the month}
  \item{mo}{numeric, month}
  \item{year}{numeric, year}
  \item{timem}{time since midnight HH.MM}
  \item{pptcum}{cumulative precipitation (mm) since midnight}
}
}
