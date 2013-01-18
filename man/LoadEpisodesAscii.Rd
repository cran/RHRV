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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
