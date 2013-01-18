\name{LoadBeatRR}
\alias{LoadBeatRR}
\title{ Builds an array of beats positions from an ascii file }
\description{
  Reads an ascii file containing RR values, i.e. distances between two successive beats.
}
\usage{
  LoadBeatRR(HRVData, RecordName, RecordPath=".", scale = 1, 
		  datetime = "1/1/1900 0:0:0", verbose = NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{ The Ascii file to be used }
  \item{RecordPath}{ The path of the file }
  \item{scale}{ 1 if beat positions in seconds or 0.001 if beat positions in milliseconds }
  \item{datetime}{ Date and time (DD/MM/YYYY HH:MM:SS) }
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
