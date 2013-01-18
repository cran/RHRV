\name{PoincarePlot}
\alias{PoincarePlot}
\title{Poincare Plot}
\usage{
  PoincarePlot(HRVData,
    indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis),
    timeLag = 1, confidenceEstimation = FALSE,
    confidence = 0.95, doPlot = FALSE,
    main = "Poincare plot", xlab = "RR[n]",
    ylab = paste(sep = "", "RR[n+", timeLag, "]"), ...)
}
\arguments{
  \item{HRVData}{Data structure that stores the beats
  register and information related to it}

  \item{indexNonLinearAnalysis}{Reference to the data
  structure that will contain the nonlinear analysis}

  \item{timeLag}{Integer denoting the number of time steps
  that will be use to construct the dependance relation:
  \eqn{RR_{j+timeLag}}{RR_(j+timeLag)} as a function of
  \eqn{RR_j}.}

  \item{confidenceEstimation}{Logical value. If TRUE, the
  ellipse-like confidence region for the two dimensional
  plot is used for fitting the ellipse and computing the
  \eqn{SD_1} and \eqn{SD_2} parameters (see details).
  Default: FALSE.}

  \item{confidence}{The confidence for computing the
  confidence region if \emph{confidenceEstimation = TRUE}.}

  \item{doPlot}{Logical value. If TRUE (default), the
  PoincarePlot is shown.}

  \item{xlab}{A title for the x axis.}

  \item{ylab}{A title for the y axis.}

  \item{main}{An overall title for the Poincare plot.}

  \item{...}{Additional parameters for the Poincare plot
  figure.}
}
\value{
  A \emph{HRVData} structure containing a
  \emph{PoincarePlot} field storing the \eqn{SD_1} and
  \eqn{SD_2} parameters. The \emph{PoincarePlot} field is
  stored under the \emph{NonLinearAnalysis} list.
}
\description{
  The Poincare plot is a graphical representation of the
  dependance between successive RR intervals obtained by
  plotting the \eqn{RR_{j+\tau}}{RR_(j+tau)} as a function
  of \eqn{RR_j}. This dependance #' is often quantified by
  fitting an ellipse to the plot. In this way, two
  parameters are obtained: \eqn{SD_1} and \eqn{SD_2}.
  \eqn{SD_1} characterizes short-term variability whereas
  that \eqn{SD_2} characterizes long-term variability.
}
\details{
  In the HRV literature, when \emph{timeLag = 1}, the
  \eqn{SD_1} and \eqn{SD_2} parameters are computed using
  time domain measures. This is the default approach in
  this function if \emph{timeLag=1}. However, sometimes the
  ellipse that is fitted using this approach is too small.
  This function also allows the user to fit a ellipse by
  estimating a confidence region (by setting
  \emph{confidenceEstimation = TRUE}). If \emph{timeLag >
  1}, the confidence region approach is always used.
}

