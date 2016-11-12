\name{LoadEpisodesAscii}
\alias{LoadEpisodesAscii}
\title{ Loads episodes file }
\description{
  Loads the information of episodes, or annotated physiological events, and stores it into the data structure containing the beat positions
}
\usage{
LoadEpisodesAscii(HRVData, FileName, RecordPath=".", Tag="", InitTime="0:0:0",
verbose=NULL,header = TRUE) 
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{FileName}{ The episodes file to be used }
  \item{RecordPath}{ The path of the file }
  \item{Tag}{ Type of episode }  
  \item{InitTime}{ Time (HH:MM:SS) }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
  \item{header}{Logical value. If TRUE, then the first line of the file is skipped. Default: TRUE.}
}
\value{
  Returns HRVData, the structure that contains beat positions register and episodes information
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
