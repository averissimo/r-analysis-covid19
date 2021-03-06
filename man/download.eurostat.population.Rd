% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{download.eurostat.population}
\alias{download.eurostat.population}
\title{Download population for EU States (by NUTS)}
\usage{
download.eurostat.population(country.code, nuts = 3)
}
\arguments{
\item{country.code}{2 letter country code}

\item{nuts}{level (1, 2 or 3)}
}
\value{
a table with country, population and nuts
}
\description{
Download population for EU States (by NUTS)
}
\examples{
download.eurostat.population('PT')
download.eurostat.population('PT', 2)
download.eurostat.population('PT', 1)
}
