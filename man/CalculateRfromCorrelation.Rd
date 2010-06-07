\name{CalculateRfromCorrelation}
\alias{CalculateRfromCorrelation}
\title{ Calculates ra and rb from Correlation }
\description{
	Calculates ra and rb distances that verify that their correlation values are Cra and Crb
}
\usage{
CalculateRfromCorrelation(HRVData, Data, m, tau, Cra, Crb)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Data}{ Portion of HRVData to be analyzed }
  \item{m}{ Value of the dimension of the expansion of data }
  \item{tau}{ Delay of the expansion of data }
  \item{Cra}{ Minimum value of correlation for calculating Fractal Dimension }
  \item{Crb}{ Maximum value of correlation for calculating Fractal Dimension }
}
\value{
  Returns a 2 by 2 matrix containing ra and rb distance in the first row and their exact correlation values in the second row
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573 (2008)
S. M. Pincus, "Approximate entropy as a measure of system complexity," Mathematics 88, 2297-2301 (1991)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculateFracDim}} 
}
\keyword{ misc }
