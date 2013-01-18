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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{SplitHRbyEpisodes}} for splitting in two parts Heart Rate Data using an specific episode type
}
\keyword{ misc }
