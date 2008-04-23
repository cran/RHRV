\name{build_hr}
\alias{build_hr}
\title{ Build the instantaneous heart rate signal from a beat position array }
\description{
  The instantaneous heart rate can be defined as the inverse of the time separation between two consecutive heart beats. Once the beats have been identified, and since the only valid values contributing to the heart rate signal are the corresponding to normal beats preceded by other normal beats, a further operation should be performed for the calculation of the instantaneous heart rate.
}
\usage{
build_hr(beat)
}
\arguments{
  \item{beat}{ the beat position array }
}
\value{
  A matrix with two columns:
     first column contains beat positions and
     second column contains associated heart rate instantaneous values
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), 21-23 June 2008.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
