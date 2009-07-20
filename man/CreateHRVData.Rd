\name{CreateHRVData}
\alias{CreateHRVData}
\title{ Creates data structure for all the calculations }
\description{
  Creates data structure that stores the beats register and all the information obtained from it
}
\usage{
CreateHRVData(verbose = FALSE)
}
\arguments{
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns HRVData, the structure that will contain beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, and one or more analysis structures
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{ \code{\link{CreateData}}, \code{\link{CreateFreqAnalysis}}  }
\keyword{ misc }
