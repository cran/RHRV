\name{ModifyEpisodes}
\alias{ModifyEpisodes}
\title{ Modifies values of episodes}
\usage{
ModifyEpisodes(HRVData, Tags=NULL, Indexes=NULL, NewInitTimes=NULL,
NewTags=NULL, NewDurations=NULL ,NewValues=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Tags}{ Vector containing types of episodes to remove }
  \item{Indexes}{ Vector containing indexes of episodes to remove (see ListEpisodes()) }
  \item{NewInitTimes}{ Vector containing new init times in seconds }
  \item{NewTags}{ Vector containing new tags for episodes }
  \item{NewDurations}{ Vector containing new durations in seconds }
  \item{NewValues}{ Vector containing new numerical values for episodes }
}
\value{
  Returns HRVData, the structure that contains beat positions register and new episodes information
}
\description{
This function allow users to modify the parameters that define episodes: Tags, InitTimes, Durations and Values.

Episodes can be selected by Tags or Indexes (or both) and more than one episodes' characteristics can be modified within the same call.

When modifying more than one episode, vectors of new parameters are recycled.

After the modification has been made, duplicate episodes are removed and they are reordered by increasing InitTimes.
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
