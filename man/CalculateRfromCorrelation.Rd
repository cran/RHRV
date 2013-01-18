\name{CalculateRfromCorrelation}
\alias{CalculateRfromCorrelation}
\title{ Calculates ra and rb from Correlation }
\description{
WARNING: \bold{deprecated} function. 
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
L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri, X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103(1):39-50, july 2011.
S. M. Pincus, "Approximate entropy as a measure of system complexity," Mathematics 88, 2297-2301 (1991)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\seealso{
  \code{\link{CalculateFracDim}} 
}
\keyword{ misc }


\note{
This function is used in the \code{\link{CalculateFracDim}} function, which is
\bold{deprecated}. We suggest the use of the \code{\link{CalculateCorrDim}} function 
instead of \code{\link{CalculateFracDim}}.
}