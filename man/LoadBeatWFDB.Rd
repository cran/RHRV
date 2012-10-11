\name{LoadBeatWFDB}
\alias{LoadBeatWFDB}
\title{ Imports data from a record in WFDB format }
\description{
  Basically, this algorithm reads the annotation file for the ECG register, and stores the information obtained in a data structure.
}
\usage{
LoadBeatWFDB(HRVData, RecordName, RecordPath = ".", annotator = "qrs", 
		verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The WFDB file to be used }
  \item{RecordPath}{ The path of the file }
  \item{annotator}{ The extension of the file }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register
} 
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
I. Garcia
}
\keyword{ IO }
\keyword{ connection }
