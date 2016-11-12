\name{BuildNIHR}
\alias{BuildNIHR}
\title{ Builds the instantaneous heart rate signal from a beat position array }
\description{
  The instantaneous heart rate can be defined as the inverse of the time separation between two consecutive heart beats. Once the beats have been identified, and since the only valid values contributing to the heart rate signal are the corresponding to normal beats preceded by other normal beats, a further operation should be performed for the calculation of the instantaneous heart rate.
}
\usage{
BuildNIHR(HRVData, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register and now associated heart rate instantaneous values also
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
