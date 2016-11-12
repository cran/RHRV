\name{RemoveEpisodes}
\alias{RemoveEpisodes}
\title{ Remove episodes by indexes or tags}
\description{
  Removes episodes from the data. Episodes can be specified using indexes or tags
}
\usage{
RemoveEpisodes(HRVData, Tags = NULL, Indexes = NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Tags}{ Vector containing types of episodes to remove }
  \item{Indexes}{ Vector containing indexes of episodes to remove (see ListEpisodes()) }
}
\value{
  Returns HRVData, without the removed episodes
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ Episodes }
\keyword{ Tags }
\keyword{ Indexes }

