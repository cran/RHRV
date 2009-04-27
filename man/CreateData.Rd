\name{CreateData}
\alias{CreateData}
\title{ Create data structure for all the calculations }
\description{
  Create data structure that stores the beats register and all the information obtained from it
}
\usage{
CreateData(verbose = FALSE)
}
\arguments{
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns Data, the structure that will contain beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, spectrogram and power of the spectrogram of the heart rate signal
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
