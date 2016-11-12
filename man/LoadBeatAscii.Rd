\name{LoadBeatAscii}
\alias{LoadBeatAscii}
\title{ Builds an array of beats positions from an ascii file }
\description{
  Reads an ascii file with data of beat positions and stores the values in a data structure. A segment of a file can be loaded making use of the "starttime" and "endtime" arguments.
}
\usage{
  LoadBeatAscii(HRVData, RecordName, RecordPath=".", scale = 1, starttime=NULL,
      endtime=NULL, datetime = "1/1/1900 0:0:0", verbose = NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{RecordName}{Ascii file containing the beats to be loaded }
  \item{RecordPath}{ The path of the file }
  \item{scale}{ 1 if beat positions in seconds or 0.001 if beat positions in milliseconds }
  \item{starttime}{Beginning of the segment of file to load}
  \item{endtime}{End of the segment of file to load}
  \item{datetime}{ Date and time (DD/MM/YYYY HH:MM:SS) }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Loads beats positions into the structure that contains RHRV information. The
  file containing the heartbeats positions must be a single column file with
  no headers. Each line should denote the occurrence time of each heartbeat. An
  example of a valid file could be the following:\cr
    \emph{0}\cr
    \emph{0.3280001}\cr
    \emph{0.7159996}\cr
    \emph{1.124}\cr
    \emph{1.5}\cr
    \emph{1.88}\cr
    (...)
} 
\references{
  L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri, X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103(1):39-50, july 2011.
}
\author{
  A. Mendez, L. Rodriguez, A. Otero, C.A. Garcia, X. Vila, M. Lado
}
\keyword{ IO }
\keyword{ connection }
