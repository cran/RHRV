\name{IntegralCorrelation}
\alias{IntegralCorrelation}
\title{ Calculates the Integral Correlation }
\description{
WARNING: \bold{deprecated} function.
The Integral correlation is calculated for every vector of the m-dimensional space
}
\usage{
IntegralCorrelation(HRVData, Data, m, tau, r)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Data}{ Portion of HRVData to be analyzed }
  \item{m}{ Value of the dimension of the expansion of data }
  \item{tau}{ Delay of the expansion of data }
  \item{r}{ Distance for calculating correlation }
}
\value{
  Returns the value of the average of IntegralCorrelations
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{ \code{\link{BuildTakensVector}}  }
\keyword{ misc }
\note{
This function is used in the \code{\link{CalculateApEn}} function, which is
\bold{deprecated}. We suggest the use of the \code{\link{CalculateSampleEntropy}}
function instead of \code{\link{CalculateApEn}}.
}