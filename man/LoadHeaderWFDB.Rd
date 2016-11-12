\name{LoadHeaderWFDB}
\alias{LoadHeaderWFDB}
\title{ Imports header information from a record in wfdb format }
\description{
  Reads the header file for the ECG register, and stores the information obtained in a data structure
}
\usage{
LoadHeaderWFDB(HRVData, RecordName, RecordPath = ".", verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The ECG file to be used }
  \item{RecordPath}{ The path of the ECG file }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns Data, the structure that contains beat positions register and data extracted from header file
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }

