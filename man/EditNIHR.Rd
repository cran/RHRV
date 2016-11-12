\name{EditNIHR}
\alias{EditNIHR}
\title{ Manually edition of non-interpolated instantaneous heart rate }
\description{
  Plots non-interpolated instantaneous heart rate for manual removing of outliers
}
\usage{
EditNIHR(HRVData, scale = 1, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{scale}{ Allows scaling for small screens }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns Data, the structure that contains beat positions register, and manually edited associated heart rate instantaneous values
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ iplot }
