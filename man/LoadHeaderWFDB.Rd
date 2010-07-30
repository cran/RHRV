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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }

