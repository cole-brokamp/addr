% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/census_geography.R
\name{tiger_states}
\alias{tiger_states}
\title{get s2_geography for census states}
\usage{
tiger_states(year)
}
\arguments{
\item{year}{vintage of TIGER/Line block group geography files}
}
\value{
a tibble with \code{GEOID} and \code{s2_geography} columns
}
\description{
get s2_geography for census states
}
\examples{
tiger_states(year = "2022")
}
