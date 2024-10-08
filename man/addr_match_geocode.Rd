% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addr_geocode.R
\name{addr_match_geocode}
\alias{addr_match_geocode}
\title{Geocode addr vectors}
\usage{
addr_match_geocode(
  x,
  ref_addr = cagis_addr()$cagis_addr,
  ref_s2,
  county = "39061",
  year = "2022"
)
}
\arguments{
\item{x}{an addr vector (or character vector of address strings) to geocode}

\item{ref_addr}{an addr vector to search for matches in}

\item{ref_s2}{a s2_cell vector of locations for each ref_addr}

\item{county}{character county identifer for TIGER street range files to search for matches in}

\item{year}{character year for TIGER street range files to search for matches in}
}
\value{
a tibble with columns: \code{addr} contains \code{x} converted to an \code{addr} vector,
\code{s2} contains the resulting geocoded s2 cells as an \code{s2cell} vector,
\code{match_method} is a factor with levels described above
}
\description{
Addresses are attempted to be matched to reference geographies using different methods
associated with decreasing levels of precision in the order listed below.
Each method generates matched s2 cell identifiers differently
and is recorded in the \code{match_method} column of the returned tibble:
\enumerate{
\item \code{ref_addr}: reference s2 cell from direct match to reference address
\item \code{tiger_range}: centroid of street-matched TIGER address ranges containing street number
\item \code{tiger_street}: centroid of street-matched TIGER address ranges closest to the street number
\item \code{none}: unmatched using all previous approaches; return missing s2 cell identifier
}
}
\details{
Performance was compared to the degauss geocoder (see \verb{/inst/compare_geocoding_to_degauss.R}) using
real-world addresses in \code{voter_addresses()}.
Match success rates were similar, but DeGAUSS matched about 5\% more of the addresses. These differences are
sensitive to the match criteria considered for DeGAUSS (here precision of 'range' & score > 0.7 \emph{or}
precision of 'street' & score > 0.55):\tabular{llrl}{
   addr_matched \tab degauss_matched \tab n \tab perc \cr
   TRUE \tab TRUE \tab 224714 \tab 92.8\% \cr
   FALSE \tab TRUE \tab 13407 \tab 5.5\% \cr
   FALSE \tab FALSE \tab 2993 \tab 1.2\% \cr
   TRUE \tab FALSE \tab 1019 \tab 0.4\% \cr
}


Among those that were geocoded by both, 97.7\% were geocoded to the same census tract, and
96.6\% to the same block group:\tabular{llrll}{
   ct_agree \tab bg_agree \tab n \tab s2_dist_ptiles (5th, 25th, 50th, 75th, 95th) \tab perc \cr
   TRUE \tab TRUE \tab 217179 \tab 14.7, 24.3, 39, 68.9, 153.6 \tab 96.6\% \cr
   FALSE \tab FALSE \tab 4805 \tab 21.6, 39.2, 158.9, 5577.9, 16998.8 \tab 2.1\% \cr
   TRUE \tab FALSE \tab 2730 \tab 19.6, 28.6, 41.2, 94.8, 571.8 \tab 1.2\% \cr
}
}
\examples{
set.seed(1)
cagis_s2 <-
  cagis_addr()$cagis_addr_data |>
  purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
  purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())
addr_match_geocode(x = sample(voter_addresses(), 100), ref_s2 = cagis_s2) |>
  print(n = 100)
}
