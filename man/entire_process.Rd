% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling-processes.R
\name{entire_process}
\alias{entire_process}
\title{Code for processing when process = "entire"}
\usage{
entire_process(method, chunkSize, count, data, dataName, replace, totalSize)
}
\arguments{
\item{method}{method}

\item{chunkSize}{int > 0 - maximal number of records to be processed at time}

\item{count}{number of samples to draw}

\item{data}{<insert doc>}

\item{dataName}{incomming data if process = "data"}

\item{replace}{whether or not to sample with replacement}

\item{totalSize}{total size}
}
\value{
function writes to Alteryx output 1
}
\description{
Code for processing when process = "entire"
}

