\name{BuildNIHR}
\alias{BuildNIHR}
\title{ Build the instantaneous heart rate signal from a beat position array }
\description{
  The instantaneous heart rate can be defined as the inverse of the time separation between two consecutive heart beats. Once the beats have been identified, and since the only valid values contributing to the heart rate signal are the corresponding to normal beats preceded by other normal beats, a further operation should be performed for the calculation of the instantaneous heart rate.
}
\usage{
BuildNIHR(Data, verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns Data, the structure that contains beat positions register and now associated heart rate instantaneous values also
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }