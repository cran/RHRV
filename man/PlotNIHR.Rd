\name{PlotNIHR}
\alias{PlotNIHR}
\title{ Simple plot of non-interpolated heart rate with configurable size and colour }
\description{
  Plots in a simple way the non-interpolated instantaneous heart rate signal
}
\usage{
PlotNIHR(HRVData, Tag = NULL, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Tag}{ Boolean argument that allows to specify if episodes, as apnoea or oxygen desaturation, contained in Data are represented by means of red boxes }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ aplot }
