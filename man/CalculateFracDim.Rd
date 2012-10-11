\name{CalculateFracDim}
\alias{CalculateFracDim}
\title{ Calculates Fractal Dimension }
\description{
   Calculates Fractal Dimension as indicated by Pincus
}
\usage{
CalculateFracDim(HRVData, indexNonLinearAnalysis = -1, m = 10, 
		tau = 3, Cra = 0.005, Crb = 0.75, N = 1000, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{indexNonLinearAnalysis}{ Reference to the data structure that will contain the non linear analysis }
  \item{m}{ Value of the dimension of the expansion of data }
  \item{tau}{ Delay of the expansion of data }
  \item{Cra}{ Minimum value of correlation for calculating Fractal Dimension }
  \item{Crb}{ Maximum value of correlation for calculating Fractal Dimension }
  \item{N}{ Number of points of the portion of signal to be analyzed }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\value{
  Returns HRVData, the structure that contains beat positions register and now associated heart rate instantaneous values also, including the value of the Fractal Dimension
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573 (2008)
S. M. Pincus, "Approximate entropy as a measure of system complexity," Mathematics 88, 2297-2301 (1991)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculateRfromCorrelation}} for finding r distance at which the correlation has a certain value
}
\keyword{ misc }
