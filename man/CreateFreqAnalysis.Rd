\name{CreateFreqAnalysis}
\alias{CreateFreqAnalysis}
\title{ Creates data analysis structure for frequency analysis calculations }
\description{
  Creates data analysis structure that stores the information extracted from a variability analysis of heart rate signal and joins it to HRVData as a member of a list
}
\usage{
CreateFreqAnalysis(HRVData, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, and a new analysis structure as a member of a list
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{ \code{\link{CreateHRVData}}  }
\keyword{ misc }
