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
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ iplot }
