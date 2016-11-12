############################## CalculateTimeLag ##########################
#' Estimate an appropiate time lag for the Takens' vectors
#' @description
#' Given a time series (timeSeries), an embedding dimension (m) and a 
#' time lag (timeLag), the \eqn{n^{th}} 
#' Takens' vector is defined as 
#' \deqn{T[n]={timeSeries[n], timeSeries[n+ timeLag],...timeSeries[n+m*timeLag]}.}
#' This function estimates an appropiate time lag by using the autocorrelation or the
#' average mutual information (AMI) function.
#' @details 
#' A basic criteria for estimating a proper time lag is based on the following reasoning:
#' if the time lag used to build the Takens' vectors is too small, the coordinates will
#' be too highly temporally correlated and the embedding will tend to cluster around 
#' the diagonal in the phase space. If the time lag is chosen too large, the resulting 
#' coordinates may be almost uncorrelated and the resulting embedding will be very complicated. 
#' Thus, the autocorrelation function can be used for  estimating an appropiate time lag of
#' a time series. However, it must be noted that the autocorrelation is a linear statistic,
#' and thus it does not take into account nonlinear dynamical correlations. To take into
#' account nonlinear correlations the average mutual information (AMI) can be used. 
#' Independently of the technique used to compute the correlation, the time lag can
#'  be selected in a variety of ways: 
#' \itemize{
#'    \item Select the time lag where the autocorrelation/AMI function decays to 0 
#'    (\emph{first.zero} method). This
#'    method is not appropriate for the AMI function, since it only takes positive values.
#'    \item Select the time lag where the autocorrelation/AMI function decays to 
#'    1/e of its value at zero (\emph{first.e.decay} method).
#'    \item Select the time lag where the autocorrelation/AMI function reaches 
#'    its first minimum (\emph{first.minimum} method).
#'    \item Select the time lag where the autocorrelation/AMI function decays to
#'     the value specified by the user (\emph{first.value} method and 
#'     \emph{value} parameter).
#' }   
#' @param HRVData Data structure that stores the beats register and information related to it.
#' @param technique The technique that we shall use to estimate the time lag. 
#' Allowed values are \emph{"acf"} and \emph{"ami"}.
#' @param method The method that we shall use to select the time lag (see the Details section). Available methods
#' are \emph{"first.zero"}, \emph{"first.e.decay"}, \emph{"first.minimum"} and \emph{"first.value"}. 
#' @param value Numeric value indicating the value that the autocorrelation/AMI function must cross in order to
#' select the time lag. It is used only with the "first.value" method.
#' @param lagMax Maximum lag at which to calculate the acf/AMI.
#' @param doPlot Logical value. If TRUE (default value), a plot of the autocorrelation/AMI function is shown.
#' @param ... Additional parameters for the \emph{acf} or the 
#' \emph{mutualInformation} functions (see \code{\link[nonlinearTseries]{mutualInformation}}).
#' @return The estimated time lag.
#' @note If the autocorrelation/AMI function does not cross the specifiged value, an error is thrown. This may be solved
#' by increasing the lag.max or selecting a higher value to which the autocorrelation/AMI function may decay.
#' 
#' This function is based on the \code{\link[nonlinearTseries]{timeLag}} function from the 
#' nonlinearTseries package.
#' @references H. Kantz  and T. Schreiber: Nonlinear Time series Analysis (Cambridge university press)
#' @examples
#' \dontrun{
#' data(HRVProcessedData)
#' HRVData = HRVProcessedData
#' HRVData = SetVerbose(HRVData,T)
#' timeLag = CalculateTimeLag(HRVData,technique = "ami")
#' embeddingDim = CalculateEmbeddingDim(HRVData,
#'                                      timeLag = timeLag,
#'                                      maxEmbeddingDim = 15)
#' }
#' @seealso \code{\link[nonlinearTseries]{timeLag}},\code{\link[nonlinearTseries]{mutualInformation}} .
CalculateTimeLag <-
  function(HRVData, technique = c("acf","ami"), 
           method = c("first.e.decay", "first.zero", 
                      "first.minimum", "first.value"), value = 1/exp(1),
           lagMax = NULL, doPlot = TRUE, ...) {
    # -------------------------------------
    # Calculates optimum time lag for embedding
    # -------------------------------------
    kMaxLag = 100
    
    VerboseMessage(HRVData$Verbose,"Calculating optimum time lag")
    
    CheckNIHR(HRVData)
    
    timeLagEstimate = timeLag(time.series=HRVData$Beat$RR,
                              technique = technique,
                              selection.method=method,value=value,
                              lag.max=lagMax,do.plot=doPlot,...)
    
    VerboseMessage(HRVData$Verbose,paste("Time Lag =", timeLagEstimate))
    
    if (timeLagEstimate > kMaxLag){
      warning(paste("Time lag is too high!! We recommend using time lag <=",kMaxLag))
    }
    return(timeLagEstimate)
  }

#privateFunction
automaticTimeLag <-function(HRVData){
  HRVData = SetVerbose(HRVData,Verbose=FALSE)
  
  acf.methods = c("first.e.decay", "first.zero","first.minimum")
  ami.methods =  c("first.e.decay","first.minimum")
  time.lags = c()
  # use acf
  for (method in acf.methods){
    tl = try({
      CalculateTimeLag(HRVData, technique="acf", method = method, 
                       lagMax = NULL, doPlot = FALSE)
    },silent=TRUE)
    if ( class(tl) != "try-error"){
      time.lags=c(time.lags,tl)
    }
  }
  # use ami
  for (method in ami.methods){
    tl = try({
      CalculateTimeLag(HRVData, technique="ami", method = method, 
                       lagMax = NULL, doPlot = FALSE)
    },silent=TRUE)
    if ( class(tl) != "try-error"){
      time.lags=c(time.lags,tl)
    }
  }
  
  if (length(time.lags)==0){
    stop("The estimation of the time lag failed! Please provide a time lag manually")
  }else{
    return(median(time.lags))
  }
  
}

#private function
automaticEstimation <- function(HRVData,timeLag,embeddingDim){
  if (is.null(timeLag)){
    VerboseMessage(HRVData$Verbose,"Estimating the Time Lag")
    timeLag = automaticTimeLag(HRVData)
    VerboseMessage(HRVData$Verbose,paste("Time Lag =", timeLag))
  }
  # automatic time lag and embedding estimation
  if(is.null(embeddingDim)){
    embeddingDim = CalculateEmbeddingDim(HRVData, timeLag = timeLag,
                                         maxEmbeddingDim=20, doPlot=FALSE)
  }
  return(c(timeLag,embeddingDim))
}

############################## EmbeddingDim ##########################
#' Estimate the proper embedding dimension for the RR time series
#' @description
#' This function determines the minimum embedding dimension from a scalar time 
#' series using the algorithm proposed by L. Cao (see references).
#' @details
#' The Cao's algorithm uses 2 functions in order to estimate the embedding dimension
#' from a time series: the E1(d) and the E2(d) functions, where d denotes the dimension.
#' 
#' E1(d) stops changing when d is greater than or equal to the embedding dimension, staying close to 1.
#' On the other hand, E2(d) is used to distinguish deterministic signals from stochastic signals. For 
#' deterministic signals, there exists some d such that E2(d)!=1. For stochastic signals,
#' E2(d) is approximately 1 for all the values. 
#' @note
#' The current implementation of this function is fully written in R, based on the 
#' \code{\link[nonlinearTseries]{estimateEmbeddingDim}} function from the 
#' nonlinearTseries package. Thus it requires 
#' heavy computations and may be quite slow. The \emph{numberPoints} parameter can be used
#' for controlling the computational burden.
#' 
#' Future versions of the package will solve this issue.
#' 
#' @param HRVData Data structure that stores the beats register and information related to it
#' @param numberPoints Number of points from the time series that will be used to estimate
#' the embedding dimension. By default, 5000 points are used.
#' @param timeLag Time lag used to build the Takens' vectors needed to estimate the
#' embedding dimension (see \link{buildTakens}). Default: 1.
#' @param maxEmbeddingDim Maximum possible embedding dimension for the time series. Default: 15.
#' @param threshold Numerical value between 0 and 1. The embedding dimension is
#' estimated using the E1(d) function. E1(d) stops changing when d is greater 
#' than or equal to embedding dimension, staying close to 1. This value 
#' establishes a threshold for considering that E1(d) is close to 1.
#' Default: 0.95
#' @param maxRelativeChange Maximum relative change in E1(d) with respect to
#' E1(d-1) in order to consider that the E1 function has been stabilized and it
#' will stop changing. Default: 0.05.  
#' @param doPlot Logical value. If TRUE (default value), a plot of E1(d) and E2(d) is shown.
#' @references 
#' Cao, L. Practical method for determining the minimum embedding dimension of a scalar time series. Physica D: Nonlinear Phenomena,
#' 110,1, pp. 43-50 (1997).
#' @examples
#' \dontrun{
#' data(HRVProcessedData)
#' HRVData = HRVProcessedData
#' HRVData = SetVerbose(HRVData,T)
#' timeLag = CalculateTimeLag(HRVData,technique = "ami")
#' embeddingDim = CalculateEmbeddingDim(HRVData,
#'                                      timeLag = timeLag,
#'                                      maxEmbeddingDim = 15)
#' }
#' @seealso \code{\link[nonlinearTseries]{estimateEmbeddingDim}}.
CalculateEmbeddingDim <-
  function(HRVData, numberPoints = 5000, timeLag = 1, maxEmbeddingDim = 15,
           threshold = 0.95, maxRelativeChange = 0.05, doPlot = TRUE){
    # -------------------------------------
    # Calculates optimum embedding dimension
    # -------------------------------------
    VerboseMessage(HRVData$Verbose,"Estimating embedding dimension")
    
    CheckNIHR(HRVData)
    
    len.RR = length(HRVData$Beat$RR)
    if (is.null(numberPoints) || (numberPoints > len.RR) ){
      numberPoints = len.RR
    }
    embeddingDim = estimateEmbeddingDim(time.series=HRVData$Beat$RR,
                                        number.points = numberPoints, 
                                        time.lag=timeLag,max.embedding.dim=maxEmbeddingDim,
                                        threshold=threshold,
                                        max.relative.change = maxRelativeChange,
                                        do.plot=doPlot)
    
    VerboseMessage(HRVData$Verbose,paste("Embedding Dimension =",embeddingDim))
    return(embeddingDim)
  }
