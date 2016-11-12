\name{WriteToFile}
\alias{WriteToFile}
\title{ Writes data structure to a file }
\description{
  Writes the data structure containing beat positions and all derived calculations to a file
}
\usage{
WriteToFile(HRVData, name, overwrite = TRUE, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{name}{ The name of the file to be used }
  \item{overwrite}{ Boolean argument for indicating what to do if the file already exists }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
