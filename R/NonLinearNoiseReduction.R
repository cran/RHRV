#' Nonlinear noise reduction
#' @description
#' Function for denoising the RR time series using nonlinear analysis techniques. 
#' @details
#' This function takes the RR time series and denoises it. The denoising
#' is achieved by averaging each Takens' vector in an m-dimensional space
#' with his neighbours (time lag=1). Each neighbourhood is specified with balls
#'  of a given radius
#' (max norm is used).
#' @param HRVData Data structure that stores the beats register and information
#'  related to it
#' @param embeddingDim Integer denoting the dimension in which we shall embed 
#' the RR time series.
#' @param radius The radius used to looking for neighbours in the phase space 
#' (see details). If the radius is not specified, a radius depending on the 
#' resolution of the RR time series is used. The resolution depends
#' on the \emph{ECGsamplingFreq} parameter. When selecting
#' the radius it must be taken into account that the RR series is specified in
#' milliseconds. 
#' @param ECGsamplingFreq The sampling frequency of the ECG from which the RR
#'  time series was derived. Although it is not necessary, if it is provided it
#'  may improve the noise reduction. If the \emph{ECGsamplingFreq} is 
#' not supplied, the sampling frequency is derived from the RR data.
#' @return A HRVData structure containing the denoised RR time series.
#' @references H. Kantz  and T. Schreiber: Nonlinear Time series Analysis (Cambridge university press)
#' @rdname nonLinearNoiseReduction
#' @note This function is based on the \code{\link[nonlinearTseries]{nonLinearNoiseReduction}} function from the 
#' nonlinearTseries package.
#' @seealso \code{\link[nonlinearTseries]{nonLinearNoiseReduction}}
NonLinearNoiseReduction <- function(HRVData, embeddingDim = NULL, radius = NULL ,
                                    ECGsamplingFreq = NULL ) {
  # -------------------------------------
  # Uses nonlinear noise reduction
  # -------------------------------------
  kSdDenoising = 3
  kthreshold = 1e-06
  
  VerboseMessage(HRVData$Verbose, "Denoising RR time series using nonlinear techniques")
  CheckNIHR(HRVData)
  
  estimations = automaticEstimation(HRVData,timeLag=1,embeddingDim)
  embeddingDim = estimations[[2]]
  
  if (is.null(ECGsamplingFreq)) {
    # Compute the RRresolution
    RRresolution = diff(unique(sort(HRVData$Beat$RR)))
    # unique() let pass small values close to zero...
    RRresolution = ifelse(RRresolution < kthreshold, 0, RRresolution)
    zero.pos = which(RRresolution == 0)
    if (length(zero.pos) > 0){
      RRresolution = RRresolution[-zero.pos]
    }
    # Let Ts be in milliseconds
    Ts = median(RRresolution)
    VerboseMessage(HRVData$Verbose,
                   paste("Estimated ECG resolution is ", rhrvFormat(Ts)," ms"))
  }else {
    # Transform Ts to milliseconds
    Ts = 1000 / ECGsamplingFreq
  }
  # Add random noise in order to avoid equal points in phase space.
  # It must be noted that if the ECG resolution is Resol (in seconds),
  # The RR resolution is 2Resol (seconds).
  noise = runif(n=length(HRVData$Beat$RR),min=-(Ts/2),max=(Ts/2))
  HRVData$Beat$RR = HRVData$Beat$RR + noise
  
  # If the user does not specify information about the power of the noise
  # the algorithm only takes into account the noise due to the finite 
  # precission of the ECG
  if (is.null(radius)){
    sdUniformDist = Ts/sqrt(12)
    radius = kSdDenoising * sdUniformDist
  }
  
  RRseries = nonLinearNoiseReduction(time.series=HRVData$Beat$RR,
                                     embedding.dim=embeddingDim,
                                     radius = radius)
  time = diffinv(tail(RRseries,-1))/1000
  time = time + HRVData$Beat$Time[[1]]
  HRVData$Beat$Time = time
  HRVData$Beat$RR = NULL
  HRVData$Beat$niHR =NULL
  
  HRVData = BuildNIHR(HRVData)
  
  return(HRVData)
}
