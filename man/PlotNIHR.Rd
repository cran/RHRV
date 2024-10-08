% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotNIHR.R
\name{PlotNIHR}
\alias{PlotNIHR}
\title{Simple plot of non-interpolated heart rate}
\usage{
PlotNIHR(
  HRVData,
  Tags = NULL,
  Indexes = NULL,
  main = "Non-interpolated instantaneous heart rate",
  xlab = "time (sec.)",
  ylab = "HR (beats/min.)",
  type = "l",
  ylim = NULL,
  Tag = NULL,
  verbose = NULL,
  ...
)
}
\arguments{
\item{HRVData}{Data structure that stores the beats register and information
related to it}

\item{Tags}{List of tags to specify which episodes, as apnoea or oxygen 
desaturation, are included in the plot. \emph{Tags}="all" plots all episodes present
in the data.}

\item{Indexes}{List of indexes to specify which episodes (see ListEpisodes),
are included in the plot. Indexes="all" plots all episodes present
in the data.}

\item{main}{A main title for the plot.}

\item{xlab}{A label for the x axis.}

\item{ylab}{a label for the y axis}

\item{type}{1-character string giving the type of plot desired. See 
\code{\link[graphics]{plot.default}}.}

\item{ylim}{The y limits of the plot.}

\item{Tag}{Deprecated argument maintained for
compatibility, use \emph{Tags} instead.}

\item{verbose}{Deprecated argument maintained for compatibility, 
use SetVerbose() instead}

\item{...}{Other graphical parameters. See 
\code{\link[graphics]{plot.default}}.}
}
\description{
Plots in a simple way the non-interpolated instantaneous heart
rate signal
}
\details{
PlotNIHR
}
\references{
L. Rodriguez-Linares, L., A.J. Mendez, M.J. Lado, D.N. Olivieri,
X.A. Vila, and I. Gomez-Conde, "An open source tool for heart rate
variability spectral analysis", Computer Methods and Programs in Biomedicine 
103(1):39-50, july 2011.
}
\author{
M. Lado, A. Mendez, D. Olivieri, L. Rodriguez, X. Vila, C.A. Garcia
}
\keyword{aplot}
