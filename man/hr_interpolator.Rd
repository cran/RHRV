\name{hr_interpolator}
\alias{hr_interpolator}
\title{ Spline interpolator for build the sample heart rate signal }
\description{
  An algorithm to obtain a heart rate signal with equally spaced values at a certain sampling frequency
}
\usage{
hr_interpolator(hr, fsamp = 4, mini = hr[1, 1], maxi = hr[length(hr)/2, 1])
}
\arguments{
  \item{hr}{ heart rate signal }
  \item{fsamp}{ sampling frequency }
  \item{mini}{ beginning of the interpolation interval }
  \item{maxi}{ end of the interpolation interval }
}
\value{
  Filtered heart rate signal matrix, equally spaced and with the necessary samples 
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
