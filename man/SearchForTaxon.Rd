% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SearchForTaxon.R
\name{SearchForTaxon}
\alias{SearchForTaxon}
\title{Search for taxon, including its descendent taxa (by default)}
\usage{
SearchForTaxon(taxon, data, other.names = NULL, db = "itis",
  descendant.ranks = c("family"))
}
\arguments{
\item{taxon}{A taxon name}

\item{data}{Data.frame from GetAllDataFromMultipleYears}

\item{other.names}{A vector of other names you want to search for, typically common names}

\item{db}{See ?taxize::downstream}

\item{descendant.ranks}{Vector of ranks to look for.}
}
\value{
A data.frame with matches.
}
\description{
Search for taxon, including its descendent taxa (by default)
}
