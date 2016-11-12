\name{BuildTakensVector}
\alias{BuildTakensVector}
\title{ Calculates Takens expanded vectors }
\description{
WARNING: \bold{deprecated} function. 
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
L. Rodriguez-Linares, A. Mendez, M. Lado, D. Olivieri, X. Vila, I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103, 39-50, doi:10.1016/j.cmpb.2010.05.012 (2011)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ misc }

\note{
This function is  \bold{deprecated}. Please use \code{\link{BuildTakens}} instead.
}
