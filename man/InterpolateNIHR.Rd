\name{InterpolateNIHR}
\alias{InterpolateNIHR}
\title{ Linear or Spline interpolator for build the sample heart rate signal }
\description{
  An algorithm to obtain a heart rate signal with equally spaced values at a certain sampling frequency
}
\usage{
InterpolateNIHR(HRVData, freqhr = 4, method = c("linear", "spline"), verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{freqhr}{ Sampling frequency }
  \item{method}{ "linear" interpolation or "spline" monotone interpolation }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register, associated heart rate instantaneous values also, and filtered heart rate signal equally spaced
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
