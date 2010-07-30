\name{SplitPowerBandByEpisodes}
\alias{SplitPowerBandByEpisodes}
\title{ Splits Power Per Band using Episodes information }
\description{
  Splits Power per Band in two lists using an specific episode type: data inside episodes and data outside episodes
}
\usage{
SplitPowerBandByEpisodes(HRVData, indexFreqAnalysis, Tag = "", verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{indexFreqAnalysis}{ Reference to the data structure that will contain the variability analysis }
  \item{Tag}{ Type of episode }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns a list with two lists: InEpisodes and OutEpisodes, both lists include ULF, VLF, LF and HF bands
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculatePowerBand}} for power calculation
}
\keyword{ misc }
