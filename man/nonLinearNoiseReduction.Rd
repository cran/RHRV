\name{NonLinearNoiseReduction}
\alias{NonLinearNoiseReduction}
\title{Nonlinear noise reduction}
\usage{
  NonLinearNoiseReduction(HRVData, embeddingDim = NULL,
    radius = NULL, ECGsamplingFreq = NULL)
}
\arguments{
  \item{HRVData}{Data structure that stores the beats
  register and information related to it}

  \item{embeddingDim}{Integer denoting the dimension in
  which we shall embed the RR time series.}

  \item{radius}{The radius used to looking for neighbours
  in the phase space (see details). If the radius is not
  specified, a radius depending on the resolution of the RR
  time series is used. The resolution depends on the
  \emph{ECGsamplingFreq} parameter. If the
  \emph{ECGsamplingFreq} is not supplied, a resolution of 1
  ms is assumed.}

  \item{ECGsamplingFreq}{The sampling frequency of the ECG
  from which the RR time series was derived. Although it is
  not necessary, if it is provided it may improve the noise
  reduction.}
}
\value{
  A HRVData structure containing the denoised RR time
  series.
}
\description{
  Function for denoising the RR time series using nonlinear
  analysis techniques.
}
\details{
  This function takes the RR time series and denoises it.
  The denoising is achieved by averaging each Takens'
  vector in an m-dimensional space with his neighbours
  (time lag=1). Each neighbourhood is specified with balls
  of a given radius (max norm is used).
}
\note{
  This function is based on the
  \code{\link[nonlinearTseries]{nonLinearNoiseReduction}}
  function from the nonlinearTseries package.
}
\author{
  Constantino A. Garcia
}
\references{
  H. Kantz and T. Schreiber: Nonlinear Time series Analysis
  (Cambridge university press)
}
\seealso{
  \code{\link[nonlinearTseries]{nonLinearNoiseReduction}}
}

