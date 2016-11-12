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
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
