\name{AddEpisodes}
\alias{AddEpisodes}
\title{ Adds new episodes manually}
\description{
  Adds information of episodes manually, or annotated physiological events, and stores it into the data structure containing the beat positions
}
\usage{
AddEpisodes(HRVData, InitTimes, Tags, Durations, Values, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{InitTimes}{ Vector containing init times in seconds }
  \item{Tags}{ Vector containing types of episodes }
  \item{Durations}{ Vector containing durations in seconds }
  \item{Values}{ Vector containing numerical values for episodes }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register and new episodes information
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
