\name{BuildTakensVector}
\alias{BuildTakensVector}
\title{ Calculates Takens expanded vectors }
\description{
   In order to calculate de Fractal Dimension and Approximate Entropy (or others properties of the data) a representation of the data in a space m-dimensional is needed
}
\usage{
BuildTakensVector(HRVData, Data, m, tau)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Data}{ Portion of HRVData to be analyzed }
  \item{m}{ Value of the dimension of the expansion of data }
  \item{tau}{ Delay of the expansion of data }
}
\value{
  Returns a matrix with the Expanded Data with N-(m-1)*tau rows (N is the length of the Data to be analyzed) and m columns
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }
