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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{AnalyzeHRbyEpisodes}} for processing Heart Rate Data using an specific episode type
}
\keyword{ misc }
