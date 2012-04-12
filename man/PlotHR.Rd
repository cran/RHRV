\name{PlotHR}
\alias{PlotHR}
\title{ Simple plot of interpolated heart rate }
\description{
  Plots in a simple way the interpolated instantaneous heart rate signal
}
\usage{
PlotHR(HRVData, Tag = NULL, verbose=NULL)
}
\arguments{
  \item{HRVData}{ Data structure that stores the beats register and information related to it }
  \item{Tag}{ List of tags to specify which episodes, as apnoea or oxygen desaturation, are included in the plot. Tag="all" plots all episodes present in the data. }
  \item{verbose}{ Deprecated argument maintained for compatibility, use SetVerbose() instead }
}
\references{
L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri, X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate variability spectral analysis", Computer Methods and Programs in Biomedicine 103(1):39-50, july 2011.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila
}
\keyword{ aplot }

