\name{GenerateEpisodes}
\alias{GenerateEpisodes}
\title{ Creates new episodes from old ones }
\description{
  Creates new episodes, or annotated physiological events, from existing ones and stores them into the data structure containing the beat positions
}
\usage{
GenerateEpisodes(HRVData, NewBegFrom, NewEndFrom, DispBeg, DispEnd, 
		OldTag = "", NewTag = "", verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{NewBegFrom}{ Source of new beginning of episodes ("Beg" for indicating the beginning as the beginning of the old episode, "End" for end)} 
  \item{NewEndFrom}{ Source of new end of episodes ("Beg" for indicating the end as the beginning of the old episode, "End" for end)}
  \item{DispBeg}{ Absolute displacement from the beginning for new episodes in seconds }
  \item{DispEnd}{ Absolute displacement from the end for new episodes in seconds }
  \item{OldTag}{ Tag of old episodes }
  \item{NewTag}{ Tag for new episodes (if empty, copies OldTag) }
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
\note{
    ##Example of arguments for creating episodes displaced one \cr
	minute before old ones:\cr
	##NewBegFrom = "Beg", NewEndFrom = "End", DispBeg = -60, \cr
		DispEnd = -60\cr
	##Example of arguments for creating episodes just after previous\cr
		ones of 1 minute length:\cr
	##NewBegFrom = "End", NewEndFrom = "End", DispBeg = 0, \cr
		DispEnd = 60\cr
}
\keyword{ misc }
