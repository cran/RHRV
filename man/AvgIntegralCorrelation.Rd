\name{AvgIntegralCorrelation}
\alias{AvgIntegralCorrelation}
\title{ Calculates the average of the Integral Correlations }
\description{
WARNING: \bold{deprecated} function. 
The Integral correlation is calculated for every vector of the m-dimensional space, and then the average of all these values is calculated
}
\usage{
AvgIntegralCorrelation(HRVData, Data, m, tau, r)
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
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{ \code{\link{IntegralCorrelation}}  }
\keyword{ misc }

\note{
This function is used in the \code{\link{CalculateApEn}} function, which is
\bold{deprecated}. We suggest the use of the \code{\link{CalculateSampleEntropy}}
function instead of \code{\link{CalculateApEn}}.
}
