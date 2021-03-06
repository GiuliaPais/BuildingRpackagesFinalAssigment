% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Create a file name from year}
\usage{
make_filename(year)
}
\arguments{
\item{year}{Single number or a vector of numbers representing the years of interest}
}
\value{
Vector of strings each following the pattern "accident_year.csv.bz2"
}
\description{
Creates a string representing a file name with the given year.
}
\examples{
\dontrun{
filename <- make_filename(2018)
filename <- make_filename(c(2016,2017,2018))
}
}
