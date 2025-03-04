% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formFunctions.R
\name{cell_extractor}
\alias{cell_extractor}
\title{This function allows building a form extactor using the simple approach of selecting the text string that occurs before and after a desired string.}
\usage{
cell_extractor(docxml, string1, string2)
}
\arguments{
\item{docxml}{The xml table for a word document.}

\item{string1}{String to start capture on.}

\item{string2}{String to end capture on.}
}
\value{
a character vector of length one
}
\description{
Read the primary table from a Gates proposal word document, keeping column names.
}
\examples{
gates3<-bind_rows(lapply(tfiles,outcome_extractor))
}

