\name{LoadBeat}
\alias{LoadBeat}
\title{ Builds an array of beats positions from different type of files }
\description{
  Reads the specific file with data of beat positions and stores the values in a data structure
}
\usage{
LoadBeat(fileType, HRVData, Recordname, RecordPath = ".", 
		annotator = "qrs", scale = 1, datetime = "1/1/1900 0:0:0", 
		annotationType = "QRS", verbose = NULL)
}
\arguments{
  \item{fileType}{ The format of the file to be used: WFDB, Ascii, RR, Polar, Suunto, EDFPlus, Ambit }
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Recordname}{ The file to be used }
  \item{RecordPath}{ The path of the file }
  \item{annotator}{ The extension of the file, only if we are working with a WFDB file }
  \item{scale}{ 1 if beat positions in seconds or 0.001 if beat positions in milliseconds, only if we are working with a RR or an Ascii file }
  \item{datetime}{ Date and time (DD/MM/YYYY HH:MM:SS), only if we are working with a RR or an Ascii file }
  \item{annotationType}{ The type of annotation wished, only if we are working with an EDF+ file }
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
