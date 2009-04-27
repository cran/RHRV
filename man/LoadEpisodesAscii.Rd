\name{LoadEpisodesAscii}
\alias{LoadEpisodesAscii}
\title{ Loads episodes file }
\description{
  Loads the information of episodes, or annotated physiological events, and stores it into the data structure containing the beat positions
}
\usage{
LoadEpisodesAscii(Data, FileName, datetime = "1/1/1900 0:0:0", verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{FileName}{ The episodes file to be used }
  \item{datetime}{ Date and time (DD/MM/YYYY HH:MM:SS) }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns Data, the structure that contains beat positions register and episodes information
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ IO }
\keyword{ connection }
