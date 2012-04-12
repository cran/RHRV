\name{InterpolateNIHR}
\alias{InterpolateNIHR}
\title{ Linear or Spline interpolator for build the sample heart rate signal }
\description{
  An algorithm to obtain a heart rate signal with equally spaced values at a certain sampling frequency
}
\usage{
InterpolateNIHR(HRVData, freqhr = 4, method="linear", verbose=NULL)
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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
