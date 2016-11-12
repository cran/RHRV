\name{SplitHRbyEpisodes}
\alias{SplitHRbyEpisodes}
\title{ Splits Heart Rate Data using Episodes information }
\description{
  Splits Heart Rate Data in two parts using an specific episode type: data inside episodes and data outside episodes
}
\usage{
SplitHRbyEpisodes(HRVData, Tag = "", verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Tag}{ Type of episode }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns a list with two vectors that is, the values of Heart Rate Data inside and outside episodes
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{AnalyzeHRbyEpisodes}} for processing Heart Rate Data using an specific episode type
}
\keyword{ misc }
