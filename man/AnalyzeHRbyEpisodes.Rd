\name{AnalyzeHRbyEpisodes}
\alias{AnalyzeHRbyEpisodes}
\title{ Analyzes Heart Rate using episodes information }
\description{
  Analyzes Heart Rate allowing to evaluate the application of a desired function inside and outside episodes
}
\usage{
AnalyzeHRbyEpisodes(HRVData, Tag="", func, ..., verbose=NULL) 
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Tag}{ Type of episode }
  \item{func}{ Function to be applied to Heart Rate Data inside and outside episodes }
  \item{...}{optional arguments to func}
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns a list with two objects, that is, the values of the application of the selected function inside and outside episodes
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{SplitHRbyEpisodes}} for splitting in two parts Heart Rate Data using an specific episode type
}
\keyword{ misc }
