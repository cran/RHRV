\name{LoadBeatAscii}
\alias{LoadBeatAscii}
\title{ Builds an array of beats positions from and ascii file }
\description{
  Reads an ascii file with data of beat positions and stores the values in a data structure }
\usage{
LoadBeatAscii(HRVData, FileName, scale = 1, datetime = "1/1/1900 0:0:0", verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{FileName}{ The ascii file to be read }
  \item{scale}{ 1 if beat positions in seconds }
  \item{datetime}{ Date and time (DD/MM/YYYY HH:MM:SS) }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns Data, the structure that contains beat positions register
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
