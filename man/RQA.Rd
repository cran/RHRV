% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RQA.R
\name{RQA}
\alias{RQA}
\title{Recurrence Quantification Analysis (RQA)}
\usage{
RQA(
  HRVData,
  indexNonLinearAnalysis = length(HRVData$NonLinearAnalysis),
  numberPoints = NULL,
  embeddingDim = NULL,
  timeLag = NULL,
  radius = 1,
  lmin = 2,
  vmin = 2,
  distanceToBorder = 2,
  doPlot = FALSE
)
}
\arguments{
\item{HRVData}{Data structure that stores the beats register and information related to it}

\item{indexNonLinearAnalysis}{Reference to the data structure that will contain the nonlinear analysis}

\item{numberPoints}{Number of points from the RR time series to be used in the RQA computation. If the number of
points is not specified, the whole RR time series is used.}

\item{embeddingDim}{Integer denoting the dimension in which we shall embed the RR time series.}

\item{timeLag}{Integer denoting the number of time steps that will be use to construct the 
Takens' vectors.}

\item{radius}{Maximum distance between two phase-space points to be considered a recurrence.}

\item{lmin}{Minimal length of a diagonal line to be considered in the RQA. Default \emph{lmin} = 2.}

\item{vmin}{Minimal length of a vertical line to be considered in the RQA. Default \emph{vmin} = 2.}

\item{distanceToBorder}{In order to avoid border effects, the \emph{distanceToBorder} points near the 
border of the recurrence matrix are ignored when computing the RQA parameters. Default, \emph{distanceToBorder} = 2.}

\item{doPlot}{Logical. If TRUE, the recurrence plot is shown. However, plotting the recurrence matrix is computationally 
expensive. Use with caution.}
}
\value{
A HRVData structure that stores an \emph{rqa} field under the NonLinearAnalysis list.
The \emph{rqa} field consist of a list with the most important RQA parameters:
\itemize{
 \item \emph{REC}: Recurrence. Percentage of recurrence points in a Recurrence Plot.
 \item \emph{DET}: Determinism. Percentage of recurrence points that form diagonal lines.
 \item \emph{LAM}: Percentage of recurrent points that form vertical lines.
 \item \emph{RATIO}: Ratio between \emph{DET} and \emph{RR}.
 \item \emph{Lmax}: Length of the longest diagonal line.
 \item \emph{Lmean}: Mean length of the diagonal lines. The main diagonal is not taken into account.
 \item \emph{DIV}: Inverse of \emph{Lmax}.
 \item \emph{Vmax}: Longest vertical line.
 \item \emph{Vmean}: Average length of the vertical lines. This parameter is also referred to as the Trapping time.
 \item \emph{ENTR}: Shannon entropy of the diagonal line lengths distribution
 \item \emph{TREND}: Trend of the number of recurrent points depending on the distance to the main diagonal
 \item \emph{diagonalHistogram}: Histogram of the length of the diagonals.
 \item \emph{recurrenceRate}: Number of recurrent points depending on the distance to the main diagonal.
}
}
\description{
The Recurrence Quantification Analysis (RQA) is an advanced technique for the nonlinear
analysis that allows to quantify the number and duration of the recurrences in the 
phase space. This function computes the RQA of the RR time series.
}
\note{
This function is based on the \code{\link[nonlinearTseries]{rqa}} function from the 
nonlinearTseries package.
}
\references{
Zbilut, J. P. and C. L. Webber. Recurrence quantification analysis. Wiley Encyclopedia of Biomedical Engineering  (2006).
}
\seealso{
\code{\link[nonlinearTseries]{rqa}}, \code{\link{RecurrencePlot}}
}
