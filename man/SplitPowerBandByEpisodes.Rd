\name{SplitPowerBandByEpisodes}
\alias{SplitPowerBandByEpisodes}
\title{ Splits Power Per Band using Episodes information }
\description{
  Splits Power per Band in two lists using an specific episode type: data inside
  episodes and data outside episodes
}
\usage{
SplitPowerBandByEpisodes(HRVData, indexFreqAnalysis  =
length(HRVData$FreqAnalysis), Tag = "", 
		verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information 
  related to it }
  \item{indexFreqAnalysis}{ Reference to the data structure that will contain 
  the variability analysis }
  \item{Tag}{ Type of episode }
  \item{verbose}{ Deprecated argument maintained for compatibility, use 
  SetVerbose() instead }
}
\value{
  Returns a list with two lists: InEpisodes and OutEpisodes, both lists include 
  ULF, VLF, LF and HF bands
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open sourcetool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculatePowerBand}} for power calculation
}
\keyword{ misc }
