% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TopicFunctions.R
\name{data_mapper}
\alias{data_mapper}
\title{Make a map based on the cliff-claven tagging}
\usage{
data_mapper(CountryPredictions, OPPORTUNITY)
}
\arguments{
\item{CountryPredictions}{object returned by PredictCountryByDoc}

\item{OPPORTUNITY}{OpID to search for}
}
\value{
data frame ready for mapping in plotly
}
\description{
Using the countrypredictions object, retuns a map for an opportunity number.
}

