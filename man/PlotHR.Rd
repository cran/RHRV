\name{PlotHR}
\alias{PlotHR}
\title{ Simple plot with configurable size and colour }
\description{
  Plots in a simple way the interpolated instantaneous heart rate signal
}
\usage{
PlotHR(Data, Episodes = FALSE, verbose = FALSE)
}
\arguments{
  \item{Data}{ Data structure that stores the beats register and all the information obtained from it }
  \item{Episodes}{ Boolean argument that allows to specify if episodes, apnoea or oxygen desaturation, contained in Data are represented by means of red boxes }
  \item{verbose}{ Boolean argument that allows to specify if the function returns additional information }
}
\references{
L. Rodriguez-Linares, X. Vila, A. Mendez, M. Lado, D. Olivieri, "RHRV: An R-based software package for heart rate variability analysis of ECG recordings," 3rd Iberian Conference in Systems and Information Technologies (CISTI 2008), Proceedings I, 565-573, ISBN: 978-84-612-4476-8 (2008)
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ aplot }
