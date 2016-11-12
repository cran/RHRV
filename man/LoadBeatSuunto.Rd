\name{LoadBeatSuunto}
\Rdversion{1.1}
\alias{LoadBeatSuunto}
\title{ Imports data from a record in Suunto format }
\description{
Reads a Suunto file with data of beat positions and stores the values in a data structure
}
\usage{
LoadBeatSuunto(HRVData, RecordName, RecordPath = ".", verbose = NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The Suunto file to be read }
  \item{RecordPath}{ The path of the file }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
Returns HRVData, the structure that contains beat positions register
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
I. Garcia
}
\keyword{ IO }
\keyword{ connection }
