\name{CreateNonLinearAnalysis}
\alias{CreateNonLinearAnalysis}
\title{ Creates data analysis structure for non linear analysis calculations }
\description{
  Creates data analysis structure that stores the information extracted from a non linear analysis of ECG signal and joins it to HRVData as a member of a list}
\usage{
CreateNonLinearAnalysis(HRVData, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register, associated heart rate instantaneous values, filtered heart rate signal equally spaced, and a new analysis structure as a member of a list
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{ \code{\link{CreateHRVData}}  }
\keyword{ misc }
