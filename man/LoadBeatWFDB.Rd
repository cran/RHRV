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
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
I. Garcia
}
\keyword{ IO }
\keyword{ connection }
