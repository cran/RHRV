\name{InterpolateNIHR}
\alias{InterpolateNIHR}
\title{ Spline interpolator for build the sample heart rate signal }
\description{
  An algorithm to obtain a heart rate signal with equally spaced values at a certain sampling frequency
}
\usage{
InterpolateNIHR(Data, freqhr = 4, verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{freqhr}{ Sampling frequency }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns Data, the structure that contains beat positions register, associated heart rate instantaneous values also, and filtered heart rate signal equally spaced
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
