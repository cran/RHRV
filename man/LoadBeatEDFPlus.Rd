\name{LoadBeatEDFPlus}
\alias{LoadBeatEDFPlus}
\title{ Imports data from a record in EDF+ format }
\description{
  Basically, this algorithm reads the annotation file for the ECG register, and stores the information obtained in a data structure.
}
\usage{
LoadBeatEDFPlus(HRVData, RecordName, RecordPath = ".", 
		annotationType ="QRS", verbose = NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The EDF+ file to be used }
  \item{RecordPath}{ The path of the file }
  \item{annotationType}{ The type of annotation wished }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register
} 
\references{
L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri, X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103(1):39-50, july 2011.
}
\author{
I. Garcia
}
\keyword{ IO }
\keyword{ connection }
