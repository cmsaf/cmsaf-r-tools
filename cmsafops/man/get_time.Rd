% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_time.R
\name{get_time}
\alias{get_time}
\title{Convert time steps to POSIXct.}
\usage{
get_time(time.unit, time.step)
}
\arguments{
\item{time.unit}{Time unit, which is conform to the CF convention
(character).}

\item{time.step}{Time steps in form of a numeric or integer vector.}
}
\value{
Time in form of POSIXct is returned. Default time zone is UTC.
}
\description{
Times in NetCDF data are generally given in form of a time step and a time
unit. This function uses both information to convert them to POSIXct time
values. For the unit 'months since' an approximation of 30.4375 d is used!
}
\examples{
get_time(time.unit = "hours since 1987-01-01", time.step = 249109)
get_time(time.unit = "days since 1987-01-01", time.step = 9109)
}
