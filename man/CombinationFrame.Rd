% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TopicFunctions.R
\name{CombinationFrame}
\alias{CombinationFrame}
\title{Keep only those annotations with tagged verbs}
\usage{
CombinationFrame(PREPFRAME)
}
\arguments{
\item{PREPFRAME}{the frame object regurned by AnnotateVerbsTopicJoin}
}
\description{
Combines sentences into blocks reducing the size of the dataframe and only keeping those that contain desired verbs. Think of it like a refilter from AnnotateVerbsTopicJoin
}

