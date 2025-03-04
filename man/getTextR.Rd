% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textFunctions.R
\name{getTextR}
\alias{getTextR}
\title{Loads and processes doc,docx,pdf,and txt files into tm corpus.}
\usage{
getTextR(fname, tika = FALSE, gen_pdf_tools = T,
  tikapath = "tika-app-1.13.jar")
}
\arguments{
\item{fname}{Directory name}

\item{tika}{Should tika be used?}

\item{gen_pdf_tools}{if using a pc, set to true. if on linux, setting to false and installing the pdf2text library will be more useful. See readPDF in the tm package for more info.}

\item{tikapath}{Path to the Tika application}
}
\value{
TM Text Corpus.
}
\description{
This command loads files into R from a directory into a corpus. Currently it reads doc, docx, pdf, and txt files.
}
\examples{
corpus1<-getTextR(file.path())
}
\seealso{
\code{\link{tm::corpus}}
}

