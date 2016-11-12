\name{SetVerbose}
\alias{SetVerbose}
\title{ Sets verbose mode on or off }
\description{
  Sets verbose mode on or off, verbose is a boolean component of the data structure HRVData that allows to specify if all the functions return additional information }
\usage{
SetVerbose(HRVData, Verbose)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
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
\keyword{ misc }
