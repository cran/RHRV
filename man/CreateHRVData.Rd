\name{CreateHRVData}
\alias{CreateHRVData}
\title{ Creates data structure for all the calculations }
\description{
  Creates data structure that stores the beats register and all the information obtained from it
}
\usage{
CreateHRVData(Verbose = FALSE)
}
\arguments{
  \item{Verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\value{
  Returns HRVData, the structure that will contain beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, and one or more analysis structures
}
\references{
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{ \code{\link{CreateFreqAnalysis}}, \code{\link{CreateTimeAnalysis}}, \code{\link{CreateNonLinearAnalysis}}  }
\keyword{ misc }

