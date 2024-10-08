% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/census_geography.R
\name{s2_join_tiger_bg}
\alias{s2_join_tiger_bg}
\title{Tiger Block Groups}
\usage{
s2_join_tiger_bg(x, year = as.character(2013:2023))
}
\arguments{
\item{x}{s2_cell vector}

\item{year}{vintage of TIGER/Line block group geography files}
}
\value{
character vector of matched census block group identifiers
}
\description{
Get the identifier of the closest census block group based on the intersection of the s2 cell locations with the
the US Census \href{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}{TIGER/Line shapefiles}
}
\examples{
s2_join_tiger_bg(x = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")), year = "2023")
}
